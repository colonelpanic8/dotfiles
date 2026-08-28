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

    # Keep Hyprland's release and separately exposed dependencies on the exact
    # revisions from the release lock. Plugins follow this input so they build
    # against the exact same Hyprland ABI.
    hyprland = {
      url = "git+https://github.com/hyprwm/Hyprland?submodules=1&rev=efb50993780079460b0cbed1363e2166a2de1d9f";
      inputs = {
        aquamarine.follows = "aquamarine";
        hyprutils.follows = "hyprutils";
      };
    };

    aquamarine = {
      url = "github:hyprwm/aquamarine/1a10fe26a9f7d989c359e6a9ea61aa2e44d06c36";
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
      url = "github:hyprwm/hyprutils/5a7b8cf221914ce4714407950e4ffbdddcd8b66f";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.systems.follows = "systems";
    };
    hyprlock = {
      url = "github:hyprwm/hyprlock/b222d9b1f87e980cac379371df57913a53b99d7f";
      inputs = {
        hyprgraphics.follows = "hyprgraphics";
        hyprlang.follows = "hyprlang";
        hyprutils.follows = "hyprutils";
        hyprwayland-scanner.follows = "hyprland/hyprwayland-scanner";
        nixpkgs.follows = "nixpkgs";
        systems.follows = "systems";
      };
    };
    xdg-desktop-portal-hyprland = {
      url = "github:hyprwm/xdg-desktop-portal-hyprland/cc8e5ef8fb2acef3db488b9a33b0c48c2a4ee204";
      inputs = {
        hyprland-protocols.follows = "hyprland/hyprland-protocols";
        hyprlang.follows = "hyprlang";
        hyprutils.follows = "hyprutils";
        hyprwayland-scanner.follows = "hyprland/hyprwayland-scanner";
        nixpkgs.follows = "nixpkgs";
        systems.follows = "systems";
      };
    };

    hyprNStack = {
      url = "github:colonelpanic8/hyprNStack?ref=codex/hyprnstack-combined-0.56";
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
      url = "github:colonelpanic8/hyprwinview/26f4a0191aa147eb47a91a9d8053fbd9bf428a24";
      inputs.hyprland.follows = "hyprland";
    };

    hyprtasking = {
      url = "github:raybbian/hyprtasking/2da6a6c73deb3ca23dc8334a9672d9a6cf403eef";
      inputs = {
        hyprland.follows = "hyprland";
        nixpkgs.follows = "nixpkgs";
        systems.follows = "systems";
      };
    };

    hypr-workspace-history = {
      url = "github:colonelpanic8/hypr-workspace-history/d1178fddea5299d166daef40235a93cb21cca2bd";
      inputs.hyprland.follows = "hyprland";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    hypr-dynamic-cursors = {
      url = "github:VirtCode/hypr-dynamic-cursors/5a224284872208b5324759d535d65061043725de";
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
      url = "github:colonelpanic8/t3code/a24dd39650b86afaa69b1d9b62042a83118510cc";
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
    perSystem = import ./flake/per-system.nix {
      inherit self inputs nixpkgs org-agenda-api agenix;
    };

    orgAgendaApiRev = builtins.substring 0 7 (org-agenda-api.rev or "unknown");
    dotfilesRev = builtins.substring 0 7 (self.rev or self.dirtyRev or "dirty");
    orgAgendaApiImageName = "org-agenda-api:colonelpanic-${orgAgendaApiRev}-${dotfilesRev}";

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
          orgAgendaApiContainer = (perSystem "x86_64-linux").packages.colonelpanic-org-agenda-api;
          inherit orgAgendaApiImageName;
        };
      };
    };
    mkConfig = {
      system ? "x86_64-linux",
      baseModules ? [],
      modules ? [],
      specialArgs ? {},
      ...
    }:
      nixpkgs.lib.nixosSystem {
        inherit system;
        modules = baseModules ++ modules;
        specialArgs =
          rec {
            inherit inputs;
            inherit machineNames;
            makeEnable = (import ./make-enable.nix) nixpkgs.lib;
            keys = import ./keys.nix;
            usersInfo = (import ./users.nix) {
              pkgs = {zsh = "zsh";};
              inherit keys system;
              inherit inputs;
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
