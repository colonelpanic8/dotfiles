{
  inputs = {
    nixpkgs = {
      url = "github:NixOS/nixpkgs/nixos-unstable";
    };

    nixpkgs-stable = {
      url = "github:NixOS/nixpkgs/nixos-26.05";
    };

    repowise = {
      url = "github:repowise-dev/repowise/73c82764c8554a26b39853602aab023c81b2d014";
      flake = false;
    };

    pyproject-nix = {
      url = "github:pyproject-nix/pyproject.nix/1b1485546d85f6f6c7aadb10c4923dbc09633263";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    uv2nix = {
      url = "github:pyproject-nix/uv2nix/0dfa8388dc855b1774f509725d8ea6806291571d";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.pyproject-nix.follows = "pyproject-nix";
    };

    pyproject-build-systems = {
      url = "github:pyproject-nix/build-system-pkgs/90fde00db3687922d39d95fc591475fd0bbbcd72";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.pyproject-nix.follows = "pyproject-nix";
      inputs.uv2nix.follows = "uv2nix";
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

    rynkbench = {
      url = "github:colonelpanic8/rynkbench";
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

    lastfm-edit = {
      url = "github:colonelpanic8/lastfm-edit";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
        rust-overlay.follows = "git-sync-rs/rust-overlay";
      };
    };

    gmcli = {
      url = "github:colonelpanic8/gmcli";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    voxtype = {
      url = "github:peteonrails/voxtype";
      inputs.nixpkgs.follows = "nixpkgs";
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

    # Pin the latest release and keep plugins on its exact ABI. Aquamarine and
    # hyprutils are exposed as shared inputs below so every plugin builds
    # against the same dependency revisions as Hyprland.
    hyprland = {
      url = "git+https://github.com/hyprwm/Hyprland?submodules=1&ref=refs/tags/v0.56.0";
      inputs = {
        aquamarine.follows = "aquamarine";
        hyprutils.follows = "hyprutils";
      };
    };

    aquamarine = {
      url = "github:hyprwm/aquamarine/9b5f14d9483445e766294eb8fbe0b8f370269ed0";
      inputs = {
        hyprutils.follows = "hyprutils";
        nixpkgs.follows = "nixpkgs";
        systems.follows = "systems";
      };
    };
    hyprcursor.follows = "hyprland/hyprcursor";
    hyprgraphics.follows = "hyprland/hyprgraphics";
    hyprlang.follows = "hyprland/hyprlang";
    hyprutils = {
      url = "github:hyprwm/hyprutils/5f03477ab3a005ff27c527486f551883535aea2f";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.systems.follows = "systems";
    };

    # v0.9.6 includes the explicit shared-pointer conversion required by the
    # hyprutils revision shipped with Hyprland 0.56.
    hyprlock = {
      url = "github:hyprwm/hyprlock/v0.9.6";
      inputs = {
        hyprgraphics.follows = "hyprgraphics";
        hyprlang.follows = "hyprlang";
        hyprutils.follows = "hyprutils";
        hyprwayland-scanner.follows = "hyprland/hyprwayland-scanner";
        nixpkgs.follows = "nixpkgs";
        systems.follows = "systems";
      };
    };

    # Use the portal revision selected by Hyprland 0.56; it includes the
    # explicit-pointer conversions required by the shared hyprutils input.
    xdph.follows = "hyprland/xdph";

    hyprNStack = {
      # Combines swapWithWindow with the Hyprland 0.56 compatibility work from
      # upstream PR #55: https://github.com/zakk4223/hyprNStack/pull/55
      url = "github:colonelpanic8/hyprNStack/1708d1da0d2ce06e357dd47ad4e82bcfcbad18c5";
      inputs = {
        hyprland.follows = "hyprland";
        nixpkgs.follows = "nixpkgs";
      };
    };

    hyprexpo = {
      # Pinned: the live-preview backend (c19cc94+) installs a shouldRenderWindow
      # hook that fires on every window/popup commit and SEGVs during normal use
      # (CPopup::visible -> shouldRenderWindow). 8bafe0d is the last commit before
      # that feature.
      url = "github:colonelpanic8/hyprexpo/8bafe0dbc7c26a3269f7e0fa01e3fa3cf54a3161";
      inputs.hyprland.follows = "hyprland";
    };

    hyprwinview = {
      url = "github:colonelpanic8/hyprwinview";
      inputs.hyprland.follows = "hyprland";
    };

    hyprtasking = {
      # Combines keyboard workspace jump labels (upstream PR #119) with the
      # Hyprland 0.56 compatibility work from upstream PR #121.
      url = "github:colonelpanic8/hyprtasking/34b34801448b353101c36382eb2658b89e66314f";
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

    hypr-dynamic-cursors = {
      # Current main follows Hyprland's post-0.56 cursor namespace move.
      url = "github:VirtCode/hypr-dynamic-cursors/5ef778ea151deb3573383d13d6e1cf7eed7336e1";
      inputs = {
        hyprland.follows = "hyprland";
        nixpkgs.follows = "nixpkgs";
      };
    };

    hyprglass = {
      url = "github:colonelpanic8/hyprglass?ref=codex/hyprland-0.55-compat";
      flake = false;
    };

    hyprsaver = {
      # Packaged locally (packages/hyprsaver); upstream's committed flake.lock
      # contains stub hashes and cannot be evaluated as a flake input.
      url = "github:maravexa/hyprsaver";
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
    };

    nixified-ai = {
      url = "github:nixified-ai/flake";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixtheplanet.url = "github:matthewcroughan/nixtheplanet";

    # No `follows` here or on claude-code-nix: both publish prebuilt binaries to
    # their own cachix, built against their own locked nixpkgs. Overriding it
    # changes the store path and forces a local rebuild of every artifact.
    codex-cli-nix = {
      url = "github:sadjow/codex-cli-nix/main";
    };

    codex-desktop-linux = {
      url = "github:ilysenko/codex-desktop-linux/main";
    };

    paseo = {
      url = "github:colonelpanic8/paseo/assembled";
    };

    hermes-agent = {
      url = "github:NousResearch/hermes-agent";
    };

    claude-code-nix = {
      url = "github:sadjow/claude-code-nix";
    };

    ccusage-fleet = {
      url = "github:Open330/ccusage-fleet/v0.3.0";
      flake = false;
    };

    # The personal assembly is generated from manifest.toml in the dedicated
    # colonelpanic8/t3code-assembly repository.
    #
    # Pin by REV, never by branch: generated assembly branches move on rebuild.
    # Each rebuild also pushes a dated tag, so older revs stay fetchable.
    t3code-integration = {
      url = "github:colonelpanic8/t3code/5a4927f359faa415c011a1971d5f7772cbec62f8";
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
          orgAgendaApiImageName = "localhost/org-agenda-api:colonelpanic-754fc33-994fd41";
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
        (builtins.mapAttrs (
            machineName: params: let
              machineParams =
                if builtins.hasAttr machineName customParams
                then (builtins.getAttr machineName customParams)
                else {};
            in
              mkConfig (params // machineParams)
          )
          defaultConfigurationParams)
        // {
          rescue = mkConfig {
            baseModules = [./machines/rescue.nix];
            specialArgs.rescueMode = "iso";
          };
          rescue-usb = mkConfig {
            baseModules = [
              ./machines/rescue.nix
              ./rescue-usb.nix
            ];
            specialArgs.rescueMode = "usb";
          };
        };
    }
    // flake-utils.lib.eachDefaultSystem perSystem;
}
