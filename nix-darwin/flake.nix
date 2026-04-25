{
  description = "Example Darwin system flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    nix-darwin.url = "github:LnL7/nix-darwin";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    railbird-secrets = {
      url = "git+ssh://gitea@dev.railbird.ai:1123/railbird/secrets-flake.git";
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

    codex-cli-nix = {
      # Default branch is `main` on GitHub (not `master`).
      url = "github:sadjow/codex-cli-nix/main";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    claude-code-nix = {
      url = "github:sadjow/claude-code-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    keepbook = {
      url = "github:colonelpanic8/keepbook";
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
    configuration = {
      pkgs,
      lib,
      config,
      ...
    }: let
      essentialPkgs = (import ../nix-shared/system/essential.nix {inherit pkgs lib inputs;}).environment.systemPackages;
    in {
      networking.hostName = "mac-demarco-mini";
      imports = [(import ./gitea-actions-runner.nix)];
      age = {
        identityPaths = [
          "${config.users.users.kat.home}/.ssh/id_ed25519"
          "/etc/ssh/ssh_host_ed25519_key"
          "/etc/ssh/ssh_host_rsa_key"
        ];
        secrets.gitea-runner-token.file = ../nixos/secrets/gitea-runner-token.mac-demarco-mini.age;
      };
      services.gitea-actions-runner = {
        user = "gitea-runner";
        instances.nix = {
          enable = true;
          name = config.networking.hostName;
          url = "https://dev.railbird.ai";
          tokenFile = config.age.secrets.gitea-runner-token.path;
          labels = [
            "nix-darwin-${pkgs.stdenv.hostPlatform.system}:host"
            "macos-aarch64-darwin"
            "nix:host"
          ];
          settings = {
            cache = {
              enabled = true;
            };
            host = {
              workdir_parent = "/var/lib/gitea-runner/action-cache-dir";
            };
          };
          hostPackages = with pkgs; [
            bash
            coreutils
            curl
            direnv
            gawk
            just
            git-lfs
            isort
            gitFull
            gnused
            ncdu
            nixVersions.stable
            nodejs
            openssh
            wget
          ];
        };
      };

      launchd.daemons.gitea-runner-nix.serviceConfig.EnvironmentVariables = {
        XDG_CONFIG_HOME = "/var/lib/gitea-runner";
        XDG_CACHE_HOME = "/var/lib/gitea-runner/.cache";
        XDG_RUNTIME_DIR = "/var/lib/gitea-runner/tmp";
      };

      system.primaryUser = "kat";

      system.defaults.NSGlobalDomain."com.apple.swipescrolldirection" = false;
      system.defaults.CustomUserPreferences."com.apple.screensaver".idleTime = 300;
      system.defaults.CustomUserPreferences."com.apple.symbolichotkeys".AppleSymbolicHotKeys = {
        "60" = {
          enabled = false;
          value = {
            parameters = [
              32
              49
              262144
            ];
            type = "standard";
          };
        };
        "61" = {
          enabled = false;
          value = {
            parameters = [
              32
              49
              786432
            ];
            type = "standard";
          };
        };
      };
      system.defaults.screensaver.askForPassword = false;
      system.defaults.screensaver.askForPasswordDelay = 0;

      power.sleep = {
        computer = "never";
        display = "never";
        harddisk = "never";
      };

      # launchd.daemons.gitea-runner-restarter = {
      #   serviceConfig = {
      #     ProgramArguments = [
      #       "/usr/bin/env"
      #       "bash"
      #       "-c"
      #       ''
      #         SERVICE_NAME="org.nixos.gitea-runner-nix"
      #         while true; do
      #         # Check the second column of launchctl list output for our service
      #         EXIT_CODE=$(sudo launchctl list | grep "$SERVICE_NAME" | awk '{print $2}')
      #         if [ -z "$EXIT_CODE" ]; then
      #         echo "$(date): $SERVICE_NAME is running correctly. Terminating the restarter."
      #         exit 0
      #         else
      #         echo "$(date): $SERVICE_NAME is not running or in error state. Attempting to restart..."
      #         sudo launchctl bootout system/$SERVICE_NAME 2>/dev/null || true
      #         sudo launchctl load /Library/LaunchDaemons/$SERVICE_NAME.plist
      #         sleep 2  # Give the service some time to start
      #         fi
      #         done
      #       ''
      #     ];
      #     RunAtLoad = true;
      #     ThrottleInterval = 300;
      #   };
      # };

      launchd.daemons.does-anything-work = {
        serviceConfig = {
          ProgramArguments = ["/usr/bin/env" "bash" "-c" "date > /var/log/does-anything-work"];
          RunAtLoad = true;
        };
      };

      nixpkgs.overlays = [
        (import ../nix-shared/overlays)
        # Use codex and claude-code from dedicated flakes with cachix
        (final: prev: {
          codex = inputs.codex-cli-nix.packages.${prev.stdenv.hostPlatform.system}.default;
          claude-code = inputs.claude-code-nix.packages.${prev.stdenv.hostPlatform.system}.default;
          git-sync-rs = git-sync-rs.packages.${prev.stdenv.hostPlatform.system}.default;
        })
      ];
      environment.systemPackages =
        essentialPkgs
        ++ [
          pkgs.gnupg
          pkgs.spotify
        ];

      nixpkgs.config.allowUnfree = true;

      # Install GUI-visible fonts into /Library/Fonts/Nix Fonts.
      fonts.packages = with pkgs; [
        nerd-fonts.jetbrains-mono
      ];

      # Homebrew casks (managed by nix-darwin, installed by nix-homebrew)
      homebrew = {
        enable = true;
        casks = [
          "codex-app"
          "ghostty"
          "raycast"
          "vlc"
        ];
        masApps = {
          Xcode = 497799835;
        };
        onActivation.cleanup = "zap";
      };

      # Auto upgrade nix package and the daemon service.
      launchd.user.envVariables.PATH = config.environment.systemPath;

      programs.direnv.enable = true;

      # Necessary for using flakes on this system.
      nix.settings = {
        experimental-features = "nix-command flakes";
        substituters = [
          "https://cache.nixos.org"
          "https://codex-cli.cachix.org"
          "https://claude-code.cachix.org"
        ];
        trusted-public-keys = [
          "codex-cli.cachix.org-1:1Br3H1hHoRYG22n//cGKJOk3cQXgYobUel6O8DgSing="
          "claude-code.cachix.org-1:YeXf2aNu7UTX8Vwrze0za1WEDS+4DuI2kVeWEE4fsRk="
        ];
      };

      # Set Git commit hash for darwin-version.
      system.configurationRevision = self.rev or self.dirtyRev or null;

      # Used for backwards compatibility, please read the changelog before changing
      system.stateVersion = 4;

      # The platform the configuration will be used on.

      nixpkgs.hostPlatform = "aarch64-darwin";
      users.users.kat.openssh.authorizedKeys.keys = inputs.railbird-secrets.keys.kanivanKeys;
      users.users.gitea-runner = {
        name = "gitea-runner";
        isHidden = false;
        home = "/Users/gitea-runner";
        createHome = false;
      };

      users.users.kat = {
        name = "kat";
        home = "/Users/kat";
      };

      programs.zsh = {
        enable = true;
        enableSyntaxHighlighting = true;
      };
      home-manager = {
        useGlobalPkgs = true;
        useUserPackages = true;
        backupFileExtension = "hm-backup";
        extraSpecialArgs = {
          inherit inputs libDir;
        };
        sharedModules = [./home/common.nix];
        users.kat = {
          imports = [./home/kat.nix];
        };
      };
    };
  in {
    darwinConfigurations."mac-demarco-mini" = nix-darwin.lib.darwinSystem {
      modules = [
        agenix.darwinModules.default
        home-manager.darwinModules.home-manager
        nix-homebrew.darwinModules.nix-homebrew
        {
          nix-homebrew = {
            enable = true;
            user = "kat";
            autoMigrate = true;
            taps = {
              "homebrew/homebrew-core" = inputs.homebrew-core;
              "homebrew/homebrew-cask" = inputs.homebrew-cask;
            };
          };
        }
        configuration
      ];
    };

    # Expose the package set, including overlays, for convenience.
    darwinPackages = self.darwinConfigurations."mac-demarco-mini".pkgs;
  };
}
