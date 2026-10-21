{
  description = "Graphical NixOS installer in dotfiles with a shared user profile";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    home-manager = {
      url = "github:nix-community/home-manager/release-26.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-utils.url = "github:numtide/flake-utils";
    agent-nixpkgs.url = "github:NixOS/nixpkgs/4975466d324710c576dc11ad614684e6bd8cad8e";
    t3code = {
      url = "github:colonelpanic8/t3code/2797e64ebf29577a6297d9f7c6fdad6538d39658";
      inputs.nixpkgs.follows = "agent-nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    codex.url = "github:sadjow/codex-cli-nix/8dc8656b82ed75c9f76002f1368415b1e8471264";
    claude.url = "github:sadjow/claude-code-nix/c932f70402686f018d829534d71f9e8ba7859cfb";
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
        specialArgs = {familyInputs = inputs;};
        modules = [
          home-manager.nixosModules.home-manager
          ./modules/user-profile.nix
          ./modules/managed-system.nix
          ./modules/agent-tools.nix
          machine
        ];
      };
    machineNames = builtins.filter (
      name: builtins.pathExists (./machines + "/${name}/default.nix")
    ) (builtins.attrNames (builtins.readDir ./machines));
    iso = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      specialArgs = {familyInputs = inputs;};
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
      testMachine = owner: updates: extra:
        mkMachine system ({...}: {
          imports = [extra];
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
        cfg = (testMachine owner false {}).config;
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
      provisionedCheck = tailscale: let
        helperKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFixtureHelperKeyOnly helper";
        cfg =
          (testMachine "other" false {
            imports = [./modules/provisioned-system.nix];
            familyInstaller.provisioning = {
              enable = true;
              authorizedKeys = [helperKey];
              inherit tailscale;
            };
          }).config;
        ssh = cfg.services.openssh;
        firewall = cfg.networking.firewall;
        provision = cfg.systemd.services.family-provision;
        autoconnect = cfg.systemd.services.tailscaled-autoconnect;
      in
        assert firewall.enable;
        assert !ssh.openFirewall;
        assert ssh.ports == [22];
        assert !(builtins.elem 22 firewall.allowedTCPPorts);
        assert firewall.trustedInterfaces == ["lo"];
        assert firewall.interfaces.tailscale0.allowedTCPPorts == [22];
        assert ssh.settings.PasswordAuthentication == false;
        assert ssh.settings.KbdInteractiveAuthentication == false;
        assert ssh.settings.PermitRootLogin == "no";
        assert cfg.users.users.dad.openssh.authorizedKeys.keys == [helperKey];
        assert cfg.users.users.root.openssh.authorizedKeys.keys == [];
        assert cfg.services.tailscale.enable;
        assert provision.serviceConfig.UMask == "0077";
        assert builtins.elem "multi-user.target" provision.wantedBy;
        assert builtins.elem "display-manager.service" provision.before;
        assert (
          if tailscale
          then
            cfg.services.tailscale.authKeyFile
            == "/run/family-installer/tailscale-authkey"
            && builtins.elem "tailscaled-autoconnect.service" provision.before
            && builtins.elem "family-provision.service" autoconnect.requires
            && builtins.elem "family-provision.service" autoconnect.after
          else cfg.services.tailscale.authKeyFile == null && !(cfg.systemd.services ? tailscaled-autoconnect)
        );
        assert builtins.all (a: a.assertion) cfg.assertions;
          pkgs.writeText "provisioned-profile-check" (builtins.toJSON {
            inherit tailscale;
            sshInterfaces = builtins.attrNames (nixpkgs.lib.filterAttrs (_: i: builtins.elem 22 i.allowedTCPPorts) firewall.interfaces);
          });
      updateConfig = (testMachine "other" true {}).config;
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
        packages = [pkgs.just pkgs.git pkgs.alejandra python pkgs.openssh pkgs.qemu pkgs.age pkgs.xorriso];
      };
      checks =
        {
          formatting = pkgs.runCommand "formatting" {nativeBuildInputs = [pkgs.alejandra];} ''
            alejandra --check ${./flake.nix} ${./modules} ${./modules/user-profile.nix}
            touch "$out"
          '';
          helper = pkgs.runCommand "installer-helper-tests" {nativeBuildInputs = [python pkgs.openssh pkgs.nix pkgs.age];} ''
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
          installer-state-version = pkgs.runCommand "installer-state-version" {} ''
            grep -F '    version = "${nixpkgs.lib.trivial.release}"' \
              ${rescueConfig.system.build.familyCalamaresExtensions}/lib/calamares/modules/nixos/main.py
            touch "$out"
          '';
          provisioned-profile = provisionedCheck false;
          provisioned-tailscale-profile = provisionedCheck true;
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
