{
  description = "Graphical NixOS installer in dotfiles with a shared user profile";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    home-manager = {
      url = "github:nix-community/home-manager/release-26.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = inputs @ {
    self,
    nixpkgs,
    home-manager,
    flake-utils,
    ...
  }: let
    mkMachine = system: machine:
      nixpkgs.lib.nixosSystem {
        inherit system;
        modules = [
          home-manager.nixosModules.home-manager
          ./modules/user-profile.nix
          ./modules/managed-system.nix
          machine
        ];
      };
    machineNames = builtins.filter (
      name: builtins.pathExists (./machines + "/${name}/default.nix")
    ) (builtins.attrNames (builtins.readDir ./machines));
    iso = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [./modules/iso.nix];
    };
  in
    {
      lib = {inherit mkMachine;};
      nixosModules = {
        default = ./modules/managed-system.nix;
        installer = ./modules/calamares.nix;
      };
      nixosConfigurations =
        (builtins.listToAttrs (map (name: {
            inherit name;
            value = mkMachine "x86_64-linux" (./machines + "/${name}/default.nix");
          })
          machineNames))
        // {installer = iso;};
    }
    // flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs {inherit system;};
      python = pkgs.python3.withPackages (ps: [ps.ruff ps.pyyaml]);
      testMachine = owner: updates:
        mkMachine system ({...}: {
          myModules.primaryUser = "dad";
          myModules.userProfile.owner = owner;
          networking.hostName = "family-test";
          system.stateVersion = "26.05";
          fileSystems."/" = {
            device = "/dev/disk/by-label/nixos";
            fsType = "ext4";
          };
          boot.loader.grub.devices = ["/dev/vda"];
          familyInstaller.updates.enable = updates;
        });
      profileCheck = owner: let
        cfg = (testMachine owner false).config;
        home = cfg.home-manager.users.dad;
        normalUsers = builtins.attrNames (nixpkgs.lib.filterAttrs (_: user: user.isNormalUser) cfg.users.users);
      in
        assert cfg.myModules.primaryUser == "dad";
        assert normalUsers == ["dad"];
        assert cfg.networking.firewall.enable;
        assert !(cfg.system.autoUpgrade.enable);
        assert !(cfg ? age);
        assert !(home.systemd.user.services ? git-sync-rs);
        assert !(home.systemd.user.services ? import-gpg-key);
        assert !(home.home.file ? ".password-store");
        assert builtins.all (a: a.assertion) cfg.assertions;
        assert (home.programs.git.settings.user or {})
        == (
          if owner == "ivan"
          then {
            name = "Ivan Malison";
            email = "IvanMalison@gmail.com";
          }
          else {}
        );
          pkgs.writeText "${owner}-profile-check" (builtins.toJSON {
            inherit normalUsers;
            primaryUser = cfg.myModules.primaryUser;
            failures = map (a: a.message) (builtins.filter (a: !a.assertion) cfg.assertions);
          });
      rescueConfig =
        (nixpkgs.lib.nixosSystem {
          inherit system;
          modules = [
            ./modules/calamares.nix
            ({...}: {
              boot.isContainer = true;
              system.stateVersion = "26.05";
              users.users.rescue-operator.isNormalUser = true;
            })
          ];
        }).config;
      rescueCheck = assert !(rescueConfig ? isoImage);
      assert !rescueConfig.services.desktopManager.plasma6.enable;
      assert !rescueConfig.services.displayManager.autoLogin.enable;
      assert rescueConfig.programs.partition-manager.enable;
      assert rescueConfig.security.polkit.enable;
      assert rescueConfig.security.wrappers.pkexec.enable;
      assert builtins.attrNames (nixpkgs.lib.filterAttrs (_: user: user.isNormalUser) rescueConfig.users.users) == ["rescue-operator"];
      assert builtins.all (a: a.assertion) rescueConfig.assertions;
        pkgs.writeText "rescue-installer-module-check" (builtins.toJSON {
          guide = rescueConfig.environment.etc."nixos-family-installer/README.md".source;
          extensions = rescueConfig.system.build.familyCalamaresExtensions;
        });
      updateConfig = (testMachine "other" true).config;
      recordUpgrade = pkgs.writeShellScript "record-upgrade" ''
        printf '%s\n' "$@" > "$out"
      '';
      upgradeScript =
        nixpkgs.lib.replaceStrings
        ["${updateConfig.system.build.nixos-rebuild}/bin/nixos-rebuild"]
        ["${recordUpgrade}"]
        updateConfig.systemd.services.nixos-upgrade.script;
    in {
      formatter = pkgs.alejandra;
      devShells.default = pkgs.mkShell {
        packages = [pkgs.just pkgs.git pkgs.alejandra python pkgs.openssh pkgs.qemu];
      };
      checks =
        {
          formatting = pkgs.runCommand "formatting" {nativeBuildInputs = [pkgs.alejandra];} ''
            alejandra --check ${./flake.nix} ${./modules} ${./modules/user-profile.nix}
            touch "$out"
          '';
          helper = pkgs.runCommand "installer-helper-tests" {nativeBuildInputs = [python pkgs.openssh pkgs.nix];} ''
            cp -r ${./scripts} scripts
            cp -r ${./tests} tests
            cp -r ${./calamares} calamares
            cp ${./pyproject.toml} pyproject.toml
            export PYTHONPATH=$PWD/scripts
            ruff check scripts tests calamares
            ruff format --check scripts tests calamares
            python -m unittest discover -s tests -v
            touch "$out"
          '';
        }
        // nixpkgs.lib.optionalAttrs pkgs.stdenv.hostPlatform.isLinux {
          other-user = profileCheck "other";
          ivan-user = profileCheck "ivan";
          rescue-module = rescueCheck;
          update-command = pkgs.runCommand "update-command" {} ''
            ${upgradeScript}
            test "$(sed -n '4p' "$out")" = '${updateConfig.system.autoUpgrade.flake}'
            test "$(wc -l < "$out")" -eq 4
          '';
        };
      packages = nixpkgs.lib.optionalAttrs (system == "x86_64-linux") {
        default = iso.config.system.build.isoImage;
        iso = iso.config.system.build.isoImage;
        calamares-extensions = iso.config.system.build.familyCalamaresExtensions;
      };
    });
}
