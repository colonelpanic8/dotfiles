{
  inputs = {
    nixpkgs = {
      url = "github:NixOS/nixpkgs/nixos-unstable";
    };

    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.systems.follows = "systems";
    };

    systems = {url = "github:nix-systems/default";};

    git-ignore-nix = {
      url = "github:hercules-ci/gitignore.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixos-hardware = {url = "github:colonelpanic8/nixos-hardware/my-master";};

    nixos-wsl = {
      url = "github:nix-community/NixOS-WSL";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    agenix = {
      url = "github:ryantm/agenix";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        home-manager.follows = "home-manager";
        systems.follows = "systems";
      };
    };

    git-sync-rs = {
      url = "github:colonelpanic8/git-sync-rs";
      inputs = {
        flake-utils.follows = "flake-utils";
        rust-overlay.inputs.nixpkgs.follows = "nixpkgs";
      };
    };

    org-agenda-api = {
      url = "github:colonelpanic8/org-agenda-api";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
        git-sync-rs.follows = "git-sync-rs";
        emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";
      };
    };

    kanshi-sni = {
      url = "github:taffybar/kanshi-sni";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };

    # Hyprland and plugins from official flakes for proper plugin compatibility
    hyprland = {
      url = "git+https://github.com/hyprwm/Hyprland?submodules=1&ref=refs/tags/v0.53.0";
    };

    hy3 = {
      url = "github:outfoxxed/hy3?ref=hl0.53.0";
      inputs.hyprland.follows = "hyprland";
    };

    hyprland-plugins = {
      url = "github:hyprwm/hyprland-plugins?ref=v0.53.0";
      inputs.hyprland.follows = "hyprland";
    };

    hyprscratch = {
      url = "github:colonelpanic8/hyprscratch/reapply-rules-on-toggle";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    railbird-secrets = {
      url = "git+ssh://gitea@dev.railbird.ai:1123/railbird/secrets-flake.git";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
        agenix.follows = "agenix";
      };
    };
    # railbird-secrets = {
    #   url = "git+ssh://gitea@dev.railbird.ai:1123/railbird/secrets-flake.git";
    # };

    xmonad = {
      url = "github:xmonad/xmonad";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        unstable.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
        git-ignore-nix.follows = "git-ignore-nix";
      };
    };

    xmonad-contrib = {
      url = "github:IvanMalison/xmonad-contrib/withMyChanges";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
        git-ignore-nix.follows = "git-ignore-nix";
        xmonad.follows = "xmonad";
      };
    };

    taffybar = {
      url = "path:/home/imalison/dotfiles/dotfiles/config/taffybar/taffybar";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
        gtk-sni-tray.follows = "gtk-sni-tray";
        gtk-strut.follows = "gtk-strut";
        status-notifier-item.follows = "status-notifier-item";
        xmonad.follows = "xmonad";
        xmonad-contrib.follows = "xmonad-contrib";
        dbus-menu.follows = "dbus-menu";
        dbus-hslogger.follows = "dbus-hslogger";
        weeder-nix.inputs.pre-commit-hooks.inputs.nixpkgs.follows = "nixpkgs";
      };
    };

    imalison-taffybar = {
      url = "path:../dotfiles/config/taffybar";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
        xmonad.follows = "xmonad";
        taffybar.follows = "taffybar";
        dbus-menu.follows = "dbus-menu";
        dbus-hslogger.follows = "dbus-hslogger";
        gtk-sni-tray.follows = "gtk-sni-tray";
        gtk-strut.follows = "gtk-strut";
        status-notifier-item.follows = "status-notifier-item";
      };
    };

    notifications-tray-icon = {
      url = "github:colonelpanic8/notifications-tray-icon";
      inputs = {
        flake-utils.follows = "flake-utils";
        git-ignore-nix.follows = "git-ignore-nix";
        nixpkgs.follows = "nixpkgs";
      };
    };

    gtk-sni-tray = {
      url = "github:taffybar/gtk-sni-tray";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
        git-ignore-nix.follows = "git-ignore-nix";
        status-notifier-item.follows = "status-notifier-item";
        gtk-strut.follows = "gtk-strut";
        dbus-menu.follows = "dbus-menu";
      };
    };

    status-notifier-item = {
      url = "github:taffybar/status-notifier-item";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
        git-ignore-nix.follows = "git-ignore-nix";
      };
    };

    gtk-strut = {
      url = "github:taffybar/gtk-strut";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
        git-ignore-nix.follows = "git-ignore-nix";
      };
    };

    dbus-menu = {
      url = "github:taffybar/dbus-menu";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
        git-ignore-nix.follows = "git-ignore-nix";
      };
    };

    dbus-hslogger = {
      url = "github:IvanMalison/dbus-hslogger";
      flake = false;
    };

    vscode-server = {
      url = "github:nix-community/nixos-vscode-server";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };

    nixified-ai = {
      url = "github:nixified-ai/flake";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixtheplanet.url = "github:matthewcroughan/nixtheplanet";

    codex-cli-nix = {
      # Default branch is `main` on GitHub (not `master`).
      url = "github:sadjow/codex-cli-nix/main";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };

    claude-code-nix = {
      url = "github:sadjow/claude-code-nix";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };

    caelestia-shell = {
      url = "github:caelestia-dots/shell";
      inputs.nixpkgs.follows = "nixpkgs";
    };

  };

  outputs = inputs @ {
    self,
    nixpkgs,
    nixos-hardware,
    home-manager,
    taffybar,
    xmonad,
    nixtheplanet,
    xmonad-contrib,
    notifications-tray-icon,
    nix,
    agenix,
    imalison-taffybar,
    hyprland,
    hy3,
    hyprland-plugins,
    org-agenda-api,
    flake-utils,
    ...
  }: let
    # Nixpkgs PR patches - just specify PR number and hash
    nixpkgsPRPatches = [
      # claude-code
      # {
      #   pr = 464698;
      #   hash = "sha256-Pe9G6b/rI0874mM7FIOSEKiaubk95NcFhTQ7paAeLTU=";
      # }
      # {
      #   pr = 464816;
      #   hash = "sha256-bKEoRy4dzP5TyRBjYskwEzr7tj8/ez/Y1XHiQgu5q5I=";
      # }
    ];

    # Custom patches that don't fit the PR template
    nixpkgsCustomPatches = [
      {
        # PR #488591: happy-coder: 0.11.2 -> 0.13.0
        url = "https://github.com/NixOS/nixpkgs/commit/72ddcd82e5c09d47358a1f9add9f85032dcfb4a8.patch";
        hash = "sha256-mlmZcwxP7IV93mQEAR3PYw8MRmNPRWXKbk9ZEKHqZc8=";
      }
    ];

    # Home-manager PR patches - just specify PR number and hash
    homeManagerPRPatches = [
      # Example:
      # {
      #   pr = 1234;
      #   hash = "sha256-...";
      # }
    ];

    # Custom home-manager patches that don't fit the PR template
    homeManagerCustomPatches = [
      {
        url = "https://github.com/colonelpanic8/home-manager/commit/92f4b7aa5254f8bcddc9ef86e04ea5314410d10b.patch";
        hash = "sha256-RQl5daVpCqQi05l9QfTEz2PpQxmsv/HYnXrgXbqbwWk=";
      }
    ];

    # Convert PR patches to full patch format for nixpkgs
    nixpkgsPrPatchesToPatches = prPatches:
      map (p: {
        url = "https://patch-diff.githubusercontent.com/raw/NixOS/nixpkgs/pull/${toString p.pr}.patch";
        hash = p.hash;
      })
      prPatches;

    # Convert PR patches to full patch format for home-manager
    homeManagerPrPatchesToPatches = prPatches:
      map (p: {
        url = "https://patch-diff.githubusercontent.com/raw/nix-community/home-manager/pull/${toString p.pr}.patch";
        hash = p.hash;
      })
      prPatches;

    # Combine all nixpkgs patches
    allNixpkgsPatches = (nixpkgsPrPatchesToPatches nixpkgsPRPatches) ++ nixpkgsCustomPatches;

    # Combine all home-manager patches
    allHomeManagerPatches = (homeManagerPrPatchesToPatches homeManagerPRPatches) ++ homeManagerCustomPatches;

    machinesFilepath = ./machines;
    machineFilenames = builtins.attrNames (builtins.readDir machinesFilepath);
    machineNameFromFilename = filename: builtins.head (builtins.split "\\." filename);
    machineNames = map machineNameFromFilename machineFilenames;
    mkConfigurationParams = filename: {
      name = machineNameFromFilename filename;
      value = {
        baseModules = [
          (machinesFilepath + ("/" + filename))
          agenix.nixosModules.default
          nixtheplanet.nixosModules.macos-ventura
        ];
      };
    };
    defaultConfigurationParams =
      builtins.listToAttrs (map mkConfigurationParams machineFilenames);
    # Build org-agenda-api container for a given system
    mkOrgAgendaApiContainerInfo = system: let
      pkgs = import nixpkgs { inherit system; };
      orgApiRev = builtins.substring 0 7 (org-agenda-api.rev or "unknown");
      dotfilesRev = builtins.substring 0 7 (self.rev or self.dirtyRev or "dirty");
      dotfilesOrgApi = import ./org-agenda-api.nix {
        inherit pkgs system inputs;
      };
      tangledConfig = dotfilesOrgApi.org-agenda-custom-config;
      containerLib = import ../org-agenda-api/container.nix {
        inherit pkgs system tangledConfig org-agenda-api orgApiRev dotfilesRev;
      };
      tag = "colonelpanic-${orgApiRev}-${dotfilesRev}";
    in {
      imageFile = containerLib.containers.colonelpanic;
      imageName = "org-agenda-api:${tag}";
    };

    customParams = {
      biskcomp = {
        system = "aarch64-linux";
      };
      air-gapped-pi = {
        system = "aarch64-linux";
      };
      railbird-sf = {
        specialArgs = let
          containerInfo = mkOrgAgendaApiContainerInfo "x86_64-linux";
        in {
          orgAgendaApiContainer = containerInfo.imageFile;
          orgAgendaApiImageName = containerInfo.imageName;
        };
      };
    };
    mkConfig = {
      system ? "x86_64-linux",
      baseModules ? [],
      modules ? [],
      specialArgs ? {},
      ...
    }: let
      # Bootstrap nixpkgs for this specific system
      bootstrapPkgs = import nixpkgs {
        inherit system;
        config = {};
        overlays = [];
      };
      # Apply patches to nixpkgs source
      patchedSource = bootstrapPkgs.applyPatches {
        name = "nixpkgs-patched";
        src = nixpkgs;
        patches = map bootstrapPkgs.fetchpatch allNixpkgsPatches;
        prePatch = ''
          mkdir -p pkgs/by-name/an/antigravity
        '';
      };
      # Get eval-config from patched source
      evalConfig = import "${patchedSource}/nixos/lib/eval-config.nix";
      # Apply patches to home-manager source (only if there are patches)
      patchedHomeManagerSource =
        if allHomeManagerPatches == []
        then home-manager
        else
          bootstrapPkgs.applyPatches {
            name = "home-manager-patched";
            src = home-manager;
            patches = map bootstrapPkgs.fetchpatch allHomeManagerPatches;
          };
      # Import the patched home-manager flake
      patchedHomeManager =
        if allHomeManagerPatches == []
        then home-manager
        else import "${patchedHomeManagerSource}/flake.nix";
      # Get the NixOS module from the patched source
      patchedHomeManagerModule =
        if allHomeManagerPatches == []
        then home-manager.nixosModules.home-manager
        else import "${patchedHomeManagerSource}/nixos";
      # Create a modified inputs with patched home-manager
      patchedInputs = inputs // {
        home-manager = inputs.home-manager // {
          nixosModules = inputs.home-manager.nixosModules // {
            home-manager = patchedHomeManagerModule;
          };
          # Also provide the patched source path for any direct imports
          outPath = patchedHomeManagerSource.outPath or "${patchedHomeManagerSource}";
        };
      };
    in
      evalConfig {
        inherit system;
        modules = baseModules ++ modules;
        specialArgs =
          rec {
            inputs = patchedInputs;
            inherit machineNames;
            makeEnable = (import ./make-enable.nix) nixpkgs.lib;
            keys = import ./keys.nix;
            usersInfo = (import ./users.nix) {
              pkgs = {zsh = "zsh";};
              inherit keys system;
              inputs = patchedInputs;
            };
            realUsers = (
              builtins.attrNames
              (nixpkgs.lib.filterAttrs
                (_: value: (builtins.elem "isNormalUser" (builtins.attrNames value)) && value.isNormalUser)
                usersInfo.users.users)
            );
          }
          // specialArgs;
      };
  in {
    nixConfig = {
      substituters = [
        "https://cache.nixos.org/"
      ];
      trusted-public-keys = [
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      ];
      extra-substituters = [
        "http://192.168.1.26:5050"
        "https://cache.flox.dev"
        "https://org-agenda-api.cachix.org"
        "https://codex-cli.cachix.org"
        "https://claude-code.cachix.org"
      ];
      extra-trusted-public-keys = [
        "1896Folsom.duckdns.org:U2FTjvP95qwAJo0oGpvmUChJCgi5zQoG1YisoI08Qoo="
        "flox-cache-public-1:7F4OyH7ZCnFhcze3fJdfyXYLQw/aV7GEed86nQ7IsOs="
        "org-agenda-api.cachix.org-1:liKFemKkOLV/rJt2txDNcpDjRsqLuBneBjkSw/UVXKA="
        "codex-cli.cachix.org-1:1Br3H1hHoRYG22n//cGKJOk3cQXgYobUel6O8DgSing="
        "claude-code.cachix.org-1:YeXf2aNu7UTX8Vwrze0za1WEDS+4DuI2kVeWEE4fsRk="
      ];
    };
    nixosConfigurations =
      builtins.mapAttrs (
        machineName: params: let
          machineParams =
            if builtins.hasAttr machineName customParams
            then (builtins.getAttr machineName customParams)
            else {};
        in
          mkConfig (params // machineParams)
      )
      defaultConfigurationParams;
  } // flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { inherit system; };

      # Get short revs for tagging
      orgApiRev = builtins.substring 0 7 (org-agenda-api.rev or "unknown");
      dotfilesRev = builtins.substring 0 7 (self.rev or self.dirtyRev or "dirty");

      # Get tangled config files from org-agenda-api.nix
      dotfilesOrgApi = import ./org-agenda-api.nix {
        inherit pkgs system;
        inherit inputs;
      };
      tangledConfig = dotfilesOrgApi.org-agenda-custom-config;

      # Import container build logic
      containerLib = import ../org-agenda-api/container.nix {
        inherit pkgs system tangledConfig org-agenda-api orgApiRev dotfilesRev;
      };
    in {
      packages = {
        colonelpanic-org-agenda-api = containerLib.containers.colonelpanic;
        kat-org-agenda-api = containerLib.containers.kat;
      };

      # Dev shell for org-agenda-api deployment
      devShells.org-agenda-api = pkgs.mkShell {
        buildInputs = [
          pkgs.flyctl
          agenix.packages.${system}.default
          pkgs.age
          pkgs.ssh-to-age
          pkgs.git
          pkgs.jq
          pkgs.just
          pkgs.curl
        ];
        shellHook = ''
          echo ""
          echo "org-agenda-api deployment shell"
          echo ""
          echo "Commands:"
          echo "  just --list             - Show available API commands"
          echo "  ./deploy.sh <instance>  - Deploy to Fly.io (colonelpanic or kat)"
          echo "  flyctl                  - Fly.io CLI"
          echo "  agenix -e <file>        - Edit encrypted secrets"
          echo ""
        '';
      };
    }
  );
}
