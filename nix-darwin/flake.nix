{
  description = "Example Darwin system flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    # Bazel 7.6.0 from current nixpkgs crashes in the Darwin bootstrap linker.
    nixpkgs-bazel.url = "github:NixOS/nixpkgs/f205b5574fd0cb7da5b702a2da51507b7f4fdd1b";

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

    flake-utils.url = "github:numtide/flake-utils";
    nix-darwin.url = "github:LnL7/nix-darwin";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-homebrew.url = "github:zhaofengli-wip/nix-homebrew";

    # Optional: Declarative tap management
    homebrew-core = {
      url = "github:homebrew/homebrew-core";
      flake = false;
    };
    homebrew-cask = {
      url = "github:homebrew/homebrew-cask";
      flake = false;
    };
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    git-sync-rs = {
      url = "github:colonelpanic8/git-sync-rs";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    gmcli = {
      url = "github:colonelpanic8/gmcli?ref=agent/json-archive-export";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    codex-cli-nix = {
      # Default branch is `main` on GitHub (not `master`).
      url = "github:sadjow/codex-cli-nix/main";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    claude-code-nix = {
      url = "github:sadjow/claude-code-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    ccusage-fleet = {
      url = "github:Open330/ccusage-fleet/v0.3.0";
      flake = false;
    };

    # Pin the assembled integration by revision; its flake owns packaging,
    # desktop integration, and the persistent server module.
    t3code-integration = {
      url = "github:colonelpanic8/t3code/f99f7b480270f853da604c39100f7e11366e8603";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };

    keepbook = {
      url = "github:colonelpanic8/keepbook";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };

    lastfm-edit = {
      url = "github:colonelpanic8/lastfm-edit";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };

    git-blame-rank = {
      url = "github:colonelpanic8/git-blame-rank";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
  };

  outputs = inputs @ {
    self,
    agenix,
    git-sync-rs,
    nix-darwin,
    nixpkgs,
    home-manager,
    nix-homebrew,
    ...
  }: let
    libDir = ../dotfiles/lib;
    personalUsers = [
      "kat"
      "imalison"
    ];
    baseHomeModules = [./home/common.nix];
    personalHomeModules = [
      ./home/personal.nix
      ./home/desktop.nix
    ];
    # Maps each Home Manager user to the modules layered over baseHomeModules.
    homeManagerConfiguration = {
      primaryUser,
      users,
    }: {lib, ...}: {
      home-manager = {
        useGlobalPkgs = true;
        useUserPackages = true;
        backupFileExtension = "hm-backup";
        extraSpecialArgs = {
          inherit inputs libDir primaryUser;
        };
        sharedModules = baseHomeModules;
        users = lib.mapAttrs (_: modules: {imports = modules;}) users;
      };
      # Home Manager refuses to activate until the user's Nix profile
      # directory exists, which a fresh account does not have yet.
      system.activationScripts.preActivation.text = lib.concatMapStrings (user: ''
        /usr/bin/sudo -u ${user} /bin/mkdir -p ${homeForUser user}/.local/state/nix/profiles
      '') (builtins.attrNames users);
    };
    homeForUser = user: "/Users/${user}";
    sharedConfiguration = {
      primaryUser,
      manageNix ? true,
    }: {
      config,
      lib,
      pkgs,
      ...
    }: {
      imports = lib.optional manageNix ./nix.nix;
      nix.enable = manageNix;
      system.primaryUser = primaryUser;
      system.keyboard = {
        enableKeyMapping = true;
        remapCapsLockToControl = true;
      };
      system.defaults.NSGlobalDomain = {
        KeyRepeat = 2;
        InitialKeyRepeat = 15;
      };
      # GUI apps only see launchd's PATH, and launchctl setenv does not expand
      # the $USER/$HOME references in systemPath.
      launchd.user.envVariables.PATH =
        builtins.replaceStrings ["$USER" "$HOME"] [primaryUser (homeForUser primaryUser)]
        config.environment.systemPath;
      environment.profiles = lib.mkForce [
        "/etc/profiles/per-user/$USER"
        "$HOME/.nix-profile"
        "/run/current-system/sw"
        "/nix/var/nix/profiles/default"
      ];
      # The uninstaller evaluates a nested nix-darwin system whose manual build
      # still passes removed nixos-render-docs flags with current nixpkgs.
      system.tools.darwin-uninstaller.enable = false;

      # SSH clients that don't forward LANG leave sessions in the C locale,
      # which makes tmux replace non-ASCII glyphs with underscores.
      environment.extraInit = ''
        export LANG="''${LANG:-en_US.UTF-8}"
      '';

      security.sudo.extraConfig = ''
        ${primaryUser} ALL=(ALL) NOPASSWD: ALL
      '';

      nixpkgs.overlays = [
        (import ../nix-shared/overlays)
        inputs.t3code-integration.overlays.client
        (import ./t3code-overlay.nix)
        # Use codex and claude-code from dedicated flakes with cachix
        (final: prev: {
          bazel = inputs.nixpkgs-bazel.legacyPackages.${prev.stdenv.hostPlatform.system}.bazel;
          starship = inputs.nixpkgs-bazel.legacyPackages.${prev.stdenv.hostPlatform.system}.starship;
          codex = inputs.codex-cli-nix.packages.${prev.stdenv.hostPlatform.system}.default;
          claude-code = inputs.claude-code-nix.packages.${prev.stdenv.hostPlatform.system}.default;
          nixos-render-docs = prev.writeShellApplication {
            name = "nixos-render-docs";
            text = ''
              args=()
              while [ "$#" -gt 0 ]; do
                case "$1" in
                  --toc-depth|--chunk-toc-depth|--section-toc-depth)
                    args+=("--sidebar-depth")
                    shift
                    if [ "$#" -gt 0 ]; then
                      args+=("$1")
                    fi
                    ;;
                  --toc-depth=*|--chunk-toc-depth=*|--section-toc-depth=*)
                    args+=("--sidebar-depth=''${1#*=}")
                    ;;
                  *)
                    args+=("$1")
                    ;;
                esac
                shift || true
              done
              exec ${prev.nixos-render-docs}/bin/nixos-render-docs "''${args[@]}"
            '';
          };
          git-sync-rs = git-sync-rs.packages.${prev.stdenv.hostPlatform.system}.default.overrideAttrs (old: {
            checkFlags =
              (old.checkFlags or [])
              ++ [
                # Git can auto-detect the Darwin Nix build user's identity, so this
                # test does not exercise git-sync-rs's missing-identity fallback here.
                "--skip"
                "sync::transport::tests::commit_retries_with_fallback_identity_when_git_identity_missing"
              ];
          });
        })
      ];

      nixpkgs.config.allowUnfree = true;

      programs.direnv.enable = true;

      # Set Git commit hash for darwin-version.
      system.configurationRevision = self.rev or self.dirtyRev or null;

      # Used for backwards compatibility, please read the changelog before changing
      system.stateVersion = lib.mkDefault 4;

      # The platform the configuration will be used on.

      nixpkgs.hostPlatform = "aarch64-darwin";

      programs.zsh = {
        enable = true;
        enableSyntaxHighlighting = true;
      };
    };
    homebrewConfiguration = {
      primaryUser,
      extraCasks ? [],
      cleanup ? "zap",
    }: {
      config,
      lib,
      ...
    }: {
      nix-homebrew = {
        enable = true;
        user = primaryUser;
        autoMigrate = true;
        taps = {
          "homebrew/homebrew-core" = inputs.homebrew-core;
          "homebrew/homebrew-cask" = inputs.homebrew-cask;
        };
      };

      # Desktop apps are managed separately from the Codex and Claude Code CLIs.
      homebrew = {
        enable = true;
        taps = builtins.attrNames config.nix-homebrew.taps;
        brews = [
          "ddcctl"
          "m1ddc"
        ];
        casks =
          [
            "claude"
            "chatgpt"
            "codex-app"
            "ghostty"
            "hammerspoon"
            "raycast"
            "spotify"
            "vlc"
          ]
          ++ extraCasks;
        greedyCasks = true;
        onActivation = {
          inherit cleanup;
          # Homebrew requires an explicit confirmation flag when
          # `brew bundle install` runs with `--cleanup`.
          extraFlags = lib.optionals (cleanup != "none") ["--force"];
        };
      };
    };
    # Run T3 Code independently of the user's GUI login session.
    t3codeConfiguration = {
      user,
      tokenSecret ? "t3code-managed-access-token",
    }: {
      config,
      lib,
      pkgs,
      ...
    }: let
      agentConfig = config.home-manager.users.${user}.launchd.agents.t3code-headless.config;
      tokenFile = config.age.secrets.${tokenSecret}.path;
      t3codeDaemon = pkgs.writeShellScript "t3code-daemon" ''
        set -eu

        attempts=0
        while [ ! -r ${lib.escapeShellArg tokenFile} ]; do
          if [ "$attempts" -ge 60 ]; then
            echo "Timed out waiting for ${tokenFile}" >&2
            exit 1
          fi
          attempts=$((attempts + 1))
          sleep 1
        done

        exec ${lib.escapeShellArgs agentConfig.ProgramArguments}
      '';
    in {
      age.secrets.${tokenSecret} = {
        file = ../nixos/secrets + "/${tokenSecret}.age";
        owner = user;
        mode = "0400";
      };

      home-manager.users.${user} = {
        services.t3code.enable = true;
        launchd.agents.t3code-headless.enable = lib.mkForce false;
      };

      launchd.daemons.t3code = {
        serviceConfig = {
          ProgramArguments = ["${t3codeDaemon}"];
          UserName = user;
          GroupName = "staff";
          WorkingDirectory = homeForUser user;
          EnvironmentVariables =
            agentConfig.EnvironmentVariables
            // {
              HOME = homeForUser user;
              USER = user;
              LOGNAME = user;
              SHELL = "/bin/zsh";
              T3CODE_ENVIRONMENT_ID = "fleet:${config.networking.hostName}";
              T3CODE_MANAGED_ACCESS_TOKEN_FILE = tokenFile;
            };
          RunAtLoad = true;
          KeepAlive = true;
          ProcessType = "Background";
          ThrottleInterval = 10;
          StandardOutPath = "${homeForUser user}/Library/Logs/t3code-daemon.log";
          StandardErrorPath = "${homeForUser user}/Library/Logs/t3code-daemon.err.log";
        };
      };
    };
    desktopConfiguration = {
      pkgs,
      config,
      lib,
      ...
    }: let
      disabledAppleSymbolicHotKey = parameters: {
        enabled = false;
        value = {
          inherit parameters;
          type = "standard";
        };
      };
    in {
      # T3 Code labels its environment with ComputerName.
      networking.computerName = config.networking.hostName;
      system.defaults.NSGlobalDomain."com.apple.swipescrolldirection" = false;
      system.defaults.CustomUserPreferences."com.apple.symbolichotkeys".AppleSymbolicHotKeys = {
        # Disable input source shortcuts that conflict with launcher usage.
        "60" = disabledAppleSymbolicHotKey [32 49 262144];
        "61" = disabledAppleSymbolicHotKey [32 49 786432];
        # Disable Spotlight's Command-Space and Finder search window shortcuts.
        "64" = disabledAppleSymbolicHotKey [32 49 1048576];
        "65" = disabledAppleSymbolicHotKey [32 49 1572864];
      };
      environment.systemPackages = [pkgs.gnupg];

      system.activationScripts.postActivation.text = lib.concatMapStringsSep "\n" (user: ''
        user_id="$(/usr/bin/id -u ${lib.escapeShellArg user})"
        if /usr/bin/pgrep -u "$user_id" -x Hammerspoon >/dev/null; then
          /bin/launchctl asuser "$user_id" /usr/bin/sudo -n -u ${lib.escapeShellArg user} \
            ${pkgs.coreutils}/bin/timeout 5 \
            /Applications/Hammerspoon.app/Contents/Frameworks/hs/hs -q -t 2 \
            -c 'hs.timer.doAfter(0.1, hs.reload)' </dev/null \
            || echo "Could not reload Hammerspoon for ${user}; reload its configuration after logging in."
        fi
      '') (builtins.attrNames config.home-manager.users);

      # Install GUI-visible fonts into /Library/Fonts/Nix Fonts.
      fonts.packages = with pkgs; [
        nerd-fonts.jetbrains-mono
      ];
    };
    mkDarwinSystem = {
      primaryUser,
      enabledHomeUsers ? [primaryUser],
      homeModules ? personalHomeModules,
      machineModule,
      manageNix ? true,
      homebrewCleanup ? "zap",
      extraCasks ? [],
      t3codeTokenSecret ? "t3code-managed-access-token",
    }:
      nix-darwin.lib.darwinSystem {
        specialArgs = {inherit inputs;};
        modules =
          [
            ../nix-shared/system/essential.nix
            ./shared-checkout.nix
            agenix.darwinModules.default
            home-manager.darwinModules.home-manager
            nix-homebrew.darwinModules.nix-homebrew
            (homebrewConfiguration {
              inherit primaryUser extraCasks;
              cleanup = homebrewCleanup;
            })
            (sharedConfiguration {inherit primaryUser manageNix;})
            (homeManagerConfiguration {
              inherit primaryUser;
              users = nixpkgs.lib.genAttrs enabledHomeUsers (_: homeModules);
            })
            desktopConfiguration
            ./camera.nix
            ./keep-awake.nix
            {
              services.keep-awake = {
                enable = true;
                onBattery = true;
              };
              services.mac-camera = {
                enable = true;
                viewer = "ivanmalison@gmail.com";
              };
            }
            machineModule
          ]
          ++ nixpkgs.lib.optional (t3codeTokenSecret != null) (t3codeConfiguration {
            user = primaryUser;
            tokenSecret = t3codeTokenSecret;
          });
      };
  in {
    darwinConfigurations."kats-macbook-neo" = mkDarwinSystem {
      primaryUser = "kat";
      machineModule = ./machines/kats-macbook-neo.nix;
      manageNix = false;
      homebrewCleanup = "none";
      homeModules = [./home/agents.nix ./home/desktop.nix];
      t3codeTokenSecret = "t3code-managed-access-token.kats-macbook-neo";
    };

    darwinConfigurations."mac-demarco-mini" = mkDarwinSystem {
      primaryUser = "imalison";
      enabledHomeUsers = personalUsers;
      machineModule = ./machines/mac-demarco-mini.nix;
    };

    darwinConfigurations."alexanders-macbook-air" = mkDarwinSystem {
      primaryUser = "alex";
      machineModule = ./machines/alexanders-macbook-air.nix;
      homebrewCleanup = "none";
      extraCasks = ["google-chrome"];
      t3codeTokenSecret = "t3code-managed-access-token.alexanders-macbook-air";
      homeModules = [
        ./home/agents.nix
        ./home/desktop.nix
        {
          dotfiles.gitIdentity = {
            name = "Alexander Malison";
            email = "alexmalison@gmail.com";
          };
        }
      ];
    };

    # Expose the package set, including overlays, for convenience.
    darwinPackages = self.darwinConfigurations."mac-demarco-mini".pkgs;
  };
}
