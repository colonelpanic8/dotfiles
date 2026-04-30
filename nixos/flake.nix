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

    # Hyprland and plugins from official flakes for proper plugin compatibility
    hyprland = {
      url = "git+https://github.com/hyprwm/Hyprland?submodules=1";
    };

    hyprNStack = {
      url = "github:colonelpanic8/hyprNStack?ref=hyprland-lua-integration";
      inputs = {
        hyprland.follows = "hyprland";
        nixpkgs.follows = "nixpkgs";
      };
    };

    hyprland-plugins-lua = {
      url = "github:colonelpanic8/hyprland-plugins?ref=codex/fix-main-ci";
      inputs.hyprland.follows = "hyprland";
    };

    hyprwinview = {
      url = "github:colonelpanic8/hyprwinview";
      inputs.hyprland.follows = "hyprland";
    };

    hypr-workspace-history = {
      url = "github:colonelpanic8/hypr-workspace-history";
      inputs.hyprland.follows = "hyprland";
      inputs.nixpkgs.follows = "nixpkgs";
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

    xmonad-river = {
      url = "path:/home/imalison/Projects/xmonad-river";
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
        nixpkgs.follows = "nixpkgs";
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

    noctalia = {
      url = "github:noctalia-dev/noctalia-shell";
      inputs.nixpkgs.follows = "nixpkgs";
    };

  };

  outputs = inputs @ {
    self,
    nixpkgs,
    nixos-hardware,
    home-manager,
    xmonad,
    nixtheplanet,
    xmonad-contrib,
    notifications-tray-icon,
    nix,
    agenix,
    imalison-taffybar,
    hyprland,
    org-agenda-api,
    flake-utils,
    ...
  }: let
    # Nixpkgs PR patches - just specify PR number and hash
    nixpkgsPRPatches = [ ];

    # Custom patches that don't fit the PR template
    nixpkgsCustomPatches = [ ];

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
        "https://ai.cachix.org"
        "https://cache.nixos-cuda.org"
        "https://nix-community.cachix.org"
        "https://cuda-maintainers.cachix.org"
        "https://numtide.cachix.org"
        "https://cache.flox.dev"
        "https://org-agenda-api.cachix.org"
        "https://colonelpanic8-dotfiles.cachix.org"
        "https://codex-cli.cachix.org"
        "https://claude-code.cachix.org"
        "https://noctalia.cachix.org"
      ];
      extra-trusted-substituters = [
        "https://ai.cachix.org"
        "https://cache.nixos.org/"
        "https://nix-community.cachix.org"
        "https://cache.nixos-cuda.org"
        "https://cuda-maintainers.cachix.org"
        "https://numtide.cachix.org"
      ];
      extra-trusted-public-keys = [
        "1896Folsom.duckdns.org:U2FTjvP95qwAJo0oGpvmUChJCgi5zQoG1YisoI08Qoo="
        "ai.cachix.org-1:N9dzRK+alWwoKXQlnn0H6aUx0lU/mspIoz8hMvGvbbc="
        "cache.nixos-cuda.org:74DUi4Ye579gUqzH4ziL9IyiJBlDpMRn9MBN8oNan9M="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "cuda-maintainers.cachix.org-1:0dq3bujKpuEPMCX6U4WylrUDZ9JyUG0VpVZa7CNfq5E="
        "numtide.cachix.org-1:2ps1kLBUWjxIneOy1Ik6cQjb41X0iXVXeHigGmycPPE="
        "flox-cache-public-1:7F4OyH7ZCnFhcze3fJdfyXYLQw/aV7GEed86nQ7IsOs="
        "org-agenda-api.cachix.org-1:liKFemKkOLV/rJt2txDNcpDjRsqLuBneBjkSw/UVXKA="
        "colonelpanic8-dotfiles.cachix.org-1:O6GF3nptpeMFapX29okzO92eSWXR36zqW6ZF2C8P0eQ="
        "codex-cli.cachix.org-1:1Br3H1hHoRYG22n//cGKJOk3cQXgYobUel6O8DgSing="
        "claude-code.cachix.org-1:YeXf2aNu7UTX8Vwrze0za1WEDS+4DuI2kVeWEE4fsRk="
        "noctalia.cachix.org-1:pCOR47nnMEo5thcxNDtzWpOxNFQsBRglJzxWPp3dkU4="
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
      lib = pkgs.lib;

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
      } // lib.optionalAttrs pkgs.stdenv.isLinux {
        hyprNStack = inputs.hyprNStack.packages.${system}.hyprNStack;
        hyprexpo-lua = inputs.hyprland-plugins-lua.packages.${system}.hyprexpo;
        hyprwinview = inputs.hyprwinview.packages.${system}.hyprwinview;
        hypr-workspace-history = inputs.hypr-workspace-history.packages.${system}.hypr-workspace-history;
      };

      checks = lib.optionalAttrs pkgs.stdenv.isLinux {
        hyprNStack = inputs.hyprNStack.packages.${system}.hyprNStack;
        hyprexpo-lua = inputs.hyprland-plugins-lua.packages.${system}.hyprexpo;
        hyprwinview = inputs.hyprwinview.packages.${system}.hyprwinview;
        hypr-workspace-history = inputs.hypr-workspace-history.packages.${system}.hypr-workspace-history;
        hyprland-config-syntax = pkgs.runCommand "hyprland-config-syntax" {
          nativeBuildInputs = [ pkgs.lua5_4 ];
        } ''
          cp ${../dotfiles/config/hypr/hyprland.lua} hyprland.lua
          luac -p hyprland.lua
          if grep -n 'hyprctl' hyprland.lua | grep -v 'hyprctl reload' | grep -v 'hyprctl dispatch hyprwinview:overview'; then
            echo "hyprland.lua should not shell out to hyprctl for window/workspace manipulation" >&2
            exit 1
          fi
          lua <<'LUA'
          local callbacks = {}

          local function noop() end

          local function dispatcher_proxy()
            local proxy = {}
            return setmetatable(proxy, {
              __index = function()
                return dispatcher_proxy()
              end,
              __call = function()
                return noop
              end,
            })
          end

          local notification = {
            is_alive = function()
              return true
            end,
            set_text = noop,
            set_timeout = noop,
            pause = noop,
            resume = noop,
            set_paused = noop,
            dismiss = noop,
          }

          local monitor = {
            id = 1,
            name = "stub-monitor",
            focused = true,
          }

          local workspace = {
            id = 1,
            name = "1",
            windows = 0,
            special = false,
            monitor = monitor,
          }

          monitor.active_workspace = workspace

          hl = {
            animation = noop,
            bind = noop,
            config = noop,
            curve = noop,
            env = noop,
            exec_cmd = noop,
            define_submap = function(_, reset_or_callback, callback)
              local cb = type(reset_or_callback) == "function" and reset_or_callback or callback
              if cb then
                cb()
              end
            end,
            monitor = noop,
            workspace_rule = noop,
            window_rule = noop,
            dsp = dispatcher_proxy(),
            notification = {
              create = function()
                return notification
              end,
            },
            plugin = {
              load = noop,
            },
            get_active_workspace = function()
              return workspace
            end,
            get_active_monitor = function()
              return monitor
            end,
            get_active_window = function()
              return nil
            end,
            get_monitor = function()
              return monitor
            end,
            get_workspace = function(id)
              if tostring(id) == "1" then
                return workspace
              end
              return nil
            end,
            get_windows = function()
              return {}
            end,
            get_workspace_windows = function()
              return {}
            end,
            on = function(_, callback)
              callbacks[#callbacks + 1] = callback
            end,
            timer = function(callback)
              callback()
              return {
                set_enabled = noop,
              }
            end,
          }

          dofile("./hyprland.lua")

          for _, callback in ipairs(callbacks) do
            callback()
          end
          LUA
          touch "$out"
        '';
        hyprland-verify-config = let
          hyprlandPackage = inputs.hyprland.packages.${system}.hyprland;
          hyprNStackPackage = inputs.hyprNStack.packages.${system}.hyprNStack;
        in pkgs.runCommand "hyprland-lua-verify-config" {} ''
          cp ${../dotfiles/config/hypr/hyprland.lua} hyprland.lua
          substituteInPlace hyprland.lua \
            --replace-fail /run/current-system/sw/lib/libhyprNStack.so \
            ${hyprNStackPackage}/lib/libhyprNStack.so
          export XDG_RUNTIME_DIR="$TMPDIR/runtime"
          mkdir -p "$XDG_RUNTIME_DIR"
          HYPRLAND_NO_CRASHREPORTER=1 ${pkgs.coreutils}/bin/timeout 20s \
            ${hyprlandPackage}/bin/Hyprland --verify-config --config "$PWD/hyprland.lua"
          touch "$out"
        '';
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
