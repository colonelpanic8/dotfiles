{
  inputs = {
    nixpkgs = {
      url = "github:NixOS/nixpkgs/nixos-unstable";
    };

    nixpkgs-stable = {
      url = "github:NixOS/nixpkgs/nixos-26.05";
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
        rust-overlay.url = "github:oxalica/rust-overlay";
        rust-overlay.inputs.nixpkgs.follows = "nixpkgs";
      };
    };

    keepbook = {
      url = "github:colonelpanic8/keepbook";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };

    git-blame-rank = {
      url = "github:colonelpanic8/git-blame-rank";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
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

    coqui-tts-streamer = {
      # Keep the default install reproducible across machines.
      # For local development, use `just switch --override-input coqui-tts-streamer path:/home/imalison/Projects/coqui-tts-streamer`.
      url = "github:colonelpanic8/coqui-tts-streamer";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
        rust-overlay.follows = "git-sync-rs/rust-overlay";
      };
    };

    kanshi-sni = {
      url = "github:taffybar/kanshi-sni";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };

    chrome-favicon-dbus = {
      url = "github:taffybar/chrome-favicon-dbus";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Hyprland and plugins from official flakes for proper plugin compatibility
    # Pin to a release tag rather than tracking main: an untagged main snapshot
    # (68e3e40, 2026-06-10) shipped a Monitor.hpp that #includes a not-yet-added
    # MonitorZoomController.hpp, breaking every plugin build (e.g. hyprexpo).
    # v0.55.2 is the release the hyprexpo fork is built against.
    hyprland = {
      url = "git+https://github.com/hyprwm/Hyprland?submodules=1&ref=refs/tags/v0.55.2";
      inputs.hyprutils.follows = "hyprutils";
    };

    aquamarine.follows = "hyprland/aquamarine";
    hyprcursor.follows = "hyprland/hyprcursor";
    hyprgraphics.follows = "hyprland/hyprgraphics";
    hyprlang.follows = "hyprland/hyprlang";
    hyprutils = {
      url = "github:hyprwm/hyprutils";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.systems.follows = "systems";
    };

    hyprNStack = {
      url = "github:colonelpanic8/hyprNStack?ref=codex/hyprnstack-combined";
      inputs = {
        hyprland.follows = "hyprland";
        nixpkgs.follows = "nixpkgs";
      };
    };

    hyprexpo = {
      # Pinned: the live-preview backend (c19cc94+) installs a shouldRenderWindow
      # hook that fires on every window/popup commit and SEGVs during normal use
      # (CPopup::visible -> shouldRenderWindow). 8bafe0d is the last commit before
      # that feature; still built against Hyprland 0.55.2.
      url = "github:colonelpanic8/hyprexpo/8bafe0dbc7c26a3269f7e0fa01e3fa3cf54a3161";
      inputs.hyprland.follows = "hyprland";
    };

    hyprwinview = {
      url = "github:colonelpanic8/hyprwinview";
      inputs.hyprland.follows = "hyprland";
    };

    hyprtasking = {
      # Upstream default branch; the Hyprland 0.55 support PR has been merged.
      url = "github:raybbian/hyprtasking";
      inputs = {
        hyprland.follows = "hyprland";
        nixpkgs.follows = "nixpkgs";
        systems.follows = "systems";
      };
    };

    hypr-workspace-history = {
      url = "github:colonelpanic8/hypr-workspace-history";
      inputs.hyprland.follows = "hyprland";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    hyprwobbly = {
      url = "github:colonelpanic8/hyprwobbly";
      inputs = {
        hyprland.follows = "hyprland";
        nixpkgs.follows = "nixpkgs";
        systems.follows = "systems";
      };
    };

    hypr-dynamic-cursors = {
      # Pinned: 98abbb6 requires Hyprland > 0.55.2 (MonitorState.hpp)
      url = "github:VirtCode/hypr-dynamic-cursors/da447486c84e0be81f2cdd208af1ef92469f0a88";
      inputs = {
        hyprland.follows = "hyprland";
        nixpkgs.follows = "nixpkgs";
      };
    };

    hyprglass = {
      url = "github:colonelpanic8/hyprglass?ref=codex/hyprland-0.55-compat";
      flake = false;
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

    xmonad = {
      url = "github:xmonad/xmonad";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        unstable.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
        git-ignore-nix.follows = "git-ignore-nix";
      };
    };

    xmonad-river = {
      url = "github:colonelpanic8/xmonad/xmonad-river";
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

    imalison-taffybar = {
      url = "path:../dotfiles/config/taffybar";
      inputs = {
        flake-utils.follows = "flake-utils";
        xmonad.follows = "xmonad";
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

    rlru = {
      url = "github:rlrml/rlru";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
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

    codex-desktop-linux = {
      url = "github:ilysenko/codex-desktop-linux";
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

    claude-desktop = {
      url = "github:aaddrick/claude-desktop-debian";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    heroic-games-launcher = {
      url = "github:colonelpanic8/HeroicGamesLauncher?ref=colonelpanic/epic-multi-account-switching";
      flake = false;
    };

    grub2-themes = {
      url = "github:vinceliuice/grub2-themes";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs @ {
    self,
    nixpkgs,
    home-manager,
    nixtheplanet,
    agenix,
    org-agenda-api,
    flake-utils,
    ...
  }: let
    # Nixpkgs PR patches - just specify PR number and hash
    nixpkgsPRPatches = [
      {
        pr = 523297;
        hash = "sha256-SsfeBuL8vIuj1R4aCRZWY5rYSlw441LK+IwLOo0cx/w=";
      }
      {
        pr = 523536;
        hash = "sha256-wzHOQAGEhnMuhjdUeTVvA78DO/cE118ehB7zfhsppqI=";
      }
      {
        pr = 523537;
        hash = "sha256-wTOj47AOXP0oryLDoT+ZJGqgcseRbk0saf77pB8/e7M=";
      }
    ];

    # Custom patches that don't fit the PR template
    nixpkgsCustomPatches = [];

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

    perSystem = import ./flake/per-system.nix {
      inherit self inputs nixpkgs org-agenda-api agenix;
    };

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
    customParams = {
      biskcomp = {
        system = "aarch64-linux";
      };
      air-gapped-pi = {
        system = "aarch64-linux";
      };
      railbird-sf = {
        specialArgs = {
          orgAgendaApiContainer = null;
          orgAgendaApiImageName = "localhost/org-agenda-api:colonelpanic-dbb8e2d-74973b2";
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
      # Get the NixOS module from the patched source
      patchedHomeManagerModule =
        if allHomeManagerPatches == []
        then home-manager.nixosModules.home-manager
        else import "${patchedHomeManagerSource}/nixos";
      # Create a modified inputs with patched home-manager
      patchedInputs =
        inputs
        // {
          home-manager =
            inputs.home-manager
            // {
              nixosModules =
                inputs.home-manager.nixosModules
                // {
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
  in
    {
      nixConfig = import ./flake/nix-config.nix;
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
    }
    // flake-utils.lib.eachDefaultSystem perSystem;
}
