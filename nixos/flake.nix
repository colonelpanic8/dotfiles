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

    nixos-wsl = {url = "github:nix-community/NixOS-WSL";};

    agenix = {url = "github:ryantm/agenix";};

    railbird-secrets = {
      url = "git+ssh://gitea@dev.railbird.ai:1123/railbird/secrets-flake.git";
    };
    # railbird-secrets = {
    #   url = "git+ssh://gitea@dev.railbird.ai:1123/railbird/secrets-flake.git";
    # };

    xmonad = {
      url = "github:xmonad/xmonad";
      inputs = {
        nixpkgs.follows = "nixpkgs";
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
      url = "github:taffybar/taffybar/old-master";
      inputs = {
        nixpkgs.follows = "nixpkgs";
      };
    };

    imalison-taffybar = {
      url = "path:../dotfiles/config/taffybar";
      # inputs = {
      #   nixpkgs.follows = "nixpkgs";
      #   flake-utils.follows = "flake-utils";
      #   xmonad.follows = "xmonad";
      #   taffybar.follows = "taffybar";
      # };
    };

    notifications-tray-icon = {
      url = "github:IvanMalison/notifications-tray-icon";
      inputs.flake-utils.follows = "flake-utils";
      inputs.git-ignore-nix.follows = "git-ignore-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    gtk-sni-tray = {
      url = "github:taffybar/gtk-sni-tray";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
        git-ignore-nix.follows = "git-ignore-nix";
        status-notifier-item.follows = "status-notifier-item";
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

    vscode-server.url = "github:nix-community/nixos-vscode-server";

    nixified-ai = {url = "github:nixified-ai/flake";};

    nixtheplanet.url = "github:matthewcroughan/nixtheplanet";
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
    ...
  }: let
    # Nixpkgs PR patches - just specify PR number and hash
    nixpkgsPRPatches = [
      {
        pr = 433540; # Rumno service PR
        hash = "sha256-8GRhLYva7+uXNIvobLFPyeQwJ2zhBrH4WCFN6B0BAJg=";
      }
      {
        pr = 434160; # git-sync-rs package
        hash = "sha256-zjzjmC1XJmwfHr/YXFyYsqUFR5MHSoxWWyxIR35YNbM=";
      }
      {
        pr = 436061;
        hash = "sha256-HZquaNBB+w5Hm5kdzvaGg7QAOgAf/EPBO7o7pKkIrMY=";
      }
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
    customPatches = [
      {
        url = "https://github.com/colonelpanic8/nixpkgs/commit/e1fc6c25b91d3d49dd02a156237721f12dbd86b2.patch";
        hash = "sha256-cKXudynZcZno5xGo7M0J9jl7ABUjZgDyhNhXrn8nBPY=";
      }
    ];

    # Convert PR patches to full patch format
    prPatchesToPatches = prPatches:
      map (p: {
        url = "https://patch-diff.githubusercontent.com/raw/NixOS/nixpkgs/pull/${toString p.pr}.patch";
        hash = p.hash;
      })
      prPatches;

    # Combine all patches
    allPatches = (prPatchesToPatches nixpkgsPRPatches) ++ customPatches;

    machinesFilepath = ./machines;
    machineFilenames = builtins.attrNames (builtins.readDir machinesFilepath);
    machineNameFromFilename = filename: builtins.head (builtins.split "\\." filename);
    machineNames = map machineNameFromFilename machineFilenames;
    mkConfigurationParams = filename: {
      name = machineNameFromFilename filename;
      value = {
        modules = [
          (machinesFilepath + ("/" + filename))
          agenix.nixosModules.default
          nixtheplanet.nixosModules.macos-ventura
        ];
      };
    };
    defaultConfigurationParams =
      builtins.listToAttrs (map mkConfigurationParams machineFilenames);
    customParams = {
      biskcomp = {
        system = "aarch64-linux";
      };
      air-gapped-pi = {
        system = "aarch64-linux";
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
        patches = map bootstrapPkgs.fetchpatch allPatches;
        prePatch = ''
          mkdir -p pkgs/by-name/an/antigravity
        '';
      };
      # Get eval-config from patched source
      evalConfig = import "${patchedSource}/nixos/lib/eval-config.nix";
    in
      evalConfig {
        inherit system;
        modules = baseModules ++ modules;
        specialArgs =
          rec {
            inherit inputs machineNames;
            makeEnable = (import ./make-enable.nix) nixpkgs.lib;
            keys = import ./keys.nix;
            usersInfo = (import ./users.nix) {
              pkgs = {zsh = "zsh";};
              inherit keys inputs system;
            };
            realUsers = (
              builtins.attrNames
              (nixpkgs.lib.filterAttrs
                (_: value: (builtins.elem "isNormalUser" (builtins.attrNames value)) && value.isNormalUser)
                usersInfo.users.users)
            );
            mapAllKeysToValue = keys: value: builtins.listToAttrs (map (name: {inherit name value;}) keys);
            forEachUser = mapAllKeysToValue realUsers;
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
      ];
      extra-trusted-public-keys = [
        "1896Folsom.duckdns.org:U2FTjvP95qwAJo0oGpvmUChJCgi5zQoG1YisoI08Qoo="
        "flox-cache-public-1:7F4OyH7ZCnFhcze3fJdfyXYLQw/aV7GEed86nQ7IsOs="
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
  };
}
