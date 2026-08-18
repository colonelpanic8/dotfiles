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
    railbird-secrets = {
      url = "git+ssh://gitea@dev.railbird.ai:1123/railbird/secrets-flake.git";
    };
    nix-homebrew.url = "github:zhaofengli-wip/nix-homebrew";
    brew-src = {
      url = "github:Homebrew/brew/6.0.16";
      flake = false;
    };

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
      url = "github:colonelpanic8/t3code/59dba05b29334e85a2a7134c3081d34b19e4d4ed";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };

    paseo = {
      url = "github:colonelpanic8/paseo/assembled";
      inputs.nixpkgs.follows = "nixpkgs";
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
    activePrimaryUser = "kat";
    targetPrimaryUser = "imalison";
    personalUsers = [
      activePrimaryUser
      targetPrimaryUser
    ];
    sharedHomeModules = [
      ./home/common.nix
      ./home/git-sync.nix
      inputs.t3code-integration.homeManagerModules.t3code-server
    ];
    homeForUser = user: "/Users/${user}";
    mkUserDescription = user:
      if user == targetPrimaryUser
      then "Ivan Malison"
      else null;
    mkConfiguration = {
      primaryUser,
      enabledHomeUsers ? [primaryUser],
    }: {
      pkgs,
      lib,
      config,
      ...
    }: let
      essentialPkgs = (import ../nix-shared/system/essential.nix {inherit pkgs lib inputs;}).environment.systemPackages;
      paseoHome = "${homeForUser primaryUser}/.paseo";
      paseoPackage = inputs.paseo.packages.${pkgs.stdenv.hostPlatform.system}.default;
      paseoDesktopPackage = inputs.paseo.packages.${pkgs.stdenv.hostPlatform.system}.desktop;
      ensurePaseoMcpInjection = import ../nix-shared/ensure-paseo-mcp-injection.nix {inherit pkgs;};
      paseoDaemon = pkgs.writeShellScript "paseo-daemon" ''
        set -eu

        ${ensurePaseoMcpInjection} ${lib.escapeShellArg "${paseoHome}/config.json"}

        secret_file='${config.age.secrets.paseo-password-environment.path}'
        /bin/wait4path "$secret_file"
        password_line="$(${pkgs.gnugrep}/bin/grep -m1 '^PASEO_PASSWORD=' "$secret_file")"
        password="''${password_line#PASEO_PASSWORD=}"
        if [ -z "$password" ]; then
          echo "Paseo password secret is empty" >&2
          exit 1
        fi

        tailscale_ip=""
        attempts=0
        while [ "$attempts" -lt 30 ]; do
          tailscale_ip="$(${config.services.tailscale.package}/bin/tailscale ip -4 2>/dev/null | ${pkgs.coreutils}/bin/head -n 1 || true)"
          if [ -n "$tailscale_ip" ]; then
            break
          fi
          attempts=$((attempts + 1))
          sleep 2
        done
        if [ -z "$tailscale_ip" ]; then
          echo "Timed out waiting for a Tailscale IPv4 address" >&2
          exit 1
        fi

        export PASEO_PASSWORD="$password"
        export PASEO_LISTEN="$tailscale_ip:6767"
        exec ${paseoPackage}/bin/paseo-server --no-relay
      '';
      disabledAppleSymbolicHotKey = parameters: {
        enabled = false;
        value = {
          inherit parameters;
          type = "standard";
        };
      };
    in {
      networking.hostName = "mac-demarco-mini";
      imports = [
        (import ./gitea-actions-runner.nix)
        (import ./gitea-runner-external-storage.nix)
      ];
      age = {
        identityPaths = [
          "${config.users.users.${primaryUser}.home}/.ssh/id_ed25519"
          "/etc/ssh/ssh_host_ed25519_key"
          "/etc/ssh/ssh_host_rsa_key"
        ];
        secrets.gitea-runner-token.file = ../nixos/secrets/gitea-runner-token.mac-demarco-mini.age;
        secrets.tailscale-authkey = {
          file = ../nixos/secrets/tailscale-authkey.age;
          owner = "root";
          mode = "0400";
        };
        secrets.paseo-password-environment = {
          file = ../nixos/secrets/paseo-password-environment.age;
          owner = primaryUser;
          mode = "0400";
        };
      };
      services.tailscale.enable = true;
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
            git
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
      launchd.daemons.tailscaled.serviceConfig.KeepAlive = true;

      launchd.daemons.tailscale-autoconnect = {
        script = ''
          set -euo pipefail

          key_file='${config.age.secrets.tailscale-authkey.path}'
          if [ ! -s "$key_file" ]; then
            exit 0
          fi
          if [ "$(cat "$key_file")" = "DISABLED" ]; then
            exit 0
          fi

          for _ in $(${pkgs.coreutils}/bin/seq 1 30); do
            state="$(${config.services.tailscale.package}/bin/tailscale status --json 2>/dev/null | ${pkgs.jq}/bin/jq -r '.BackendState // empty' || true)"
            if [ "$state" = "Running" ]; then
              exit 0
            fi
            if [ -n "$state" ]; then
              break
            fi
            sleep 2
          done

          ${config.services.tailscale.package}/bin/tailscale up \
            --auth-key "file:$key_file" \
            --accept-dns=true \
            --operator="${primaryUser}" \
            --timeout=60s
        '';
        serviceConfig = {
          RunAtLoad = true;
          StartInterval = 300;
          StandardOutPath = "/var/log/tailscale-autoconnect.log";
          StandardErrorPath = "/var/log/tailscale-autoconnect.err.log";
        };
      };

      launchd.daemons.paseo = {
        serviceConfig = {
          ProgramArguments = ["${paseoDaemon}"];
          UserName = primaryUser;
          GroupName = "staff";
          WorkingDirectory = homeForUser primaryUser;
          EnvironmentVariables = {
            HOME = homeForUser primaryUser;
            USER = primaryUser;
            LOGNAME = primaryUser;
            SHELL = "/bin/zsh";
            NODE_ENV = "production";
            PASEO_HOME = paseoHome;
            PASEO_HOSTNAMES = config.networking.hostName;
            PATH = lib.concatStringsSep ":" [
              "${homeForUser primaryUser}/.nix-profile/bin"
              "${homeForUser primaryUser}/.local/state/nix/profile/bin"
              "/etc/profiles/per-user/${primaryUser}/bin"
              "/run/current-system/sw/bin"
              "/nix/var/nix/profiles/default/bin"
              "/opt/homebrew/bin"
              "/usr/local/bin"
              "/usr/bin"
              "/bin"
            ];
          };
          RunAtLoad = true;
          KeepAlive = true;
          ProcessType = "Background";
          ThrottleInterval = 10;
          StandardOutPath = "${homeForUser primaryUser}/Library/Logs/paseo-daemon.log";
          StandardErrorPath = "${homeForUser primaryUser}/Library/Logs/paseo-daemon.err.log";
        };
      };

      system.primaryUser = primaryUser;
      # The uninstaller evaluates a nested nix-darwin system whose manual build
      # still passes removed nixos-render-docs flags with current nixpkgs.
      system.tools.darwin-uninstaller.enable = false;

      security.sudo.extraConfig = ''
        ${primaryUser} ALL=(ALL) NOPASSWD: ALL
      '';

      system.defaults.NSGlobalDomain."com.apple.swipescrolldirection" = false;
      system.defaults.CustomUserPreferences."com.apple.screensaver".idleTime = 0;
      system.defaults.CustomUserPreferences."com.apple.symbolichotkeys".AppleSymbolicHotKeys = {
        # Disable input source shortcuts that conflict with launcher usage.
        "60" = disabledAppleSymbolicHotKey [32 49 262144];
        "61" = disabledAppleSymbolicHotKey [32 49 786432];
        # Disable Spotlight's Command-Space and Finder search window shortcuts.
        "64" = disabledAppleSymbolicHotKey [32 49 1048576];
        "65" = disabledAppleSymbolicHotKey [32 49 1572864];
      };
      system.defaults.screensaver.askForPassword = false;
      system.defaults.screensaver.askForPasswordDelay = 0;

      system.activationScripts.postActivation.text = ''
        echo >&2 "current-host screensaver defaults..."
        primary_uid="$(id -u -- ${primaryUser})"
        if launchctl print "gui/$primary_uid" >/dev/null 2>&1; then
          launchctl asuser "$primary_uid" sudo --user=${primaryUser} -- defaults -currentHost write com.apple.screensaver askForPassword -bool false
          launchctl asuser "$primary_uid" sudo --user=${primaryUser} -- defaults -currentHost write com.apple.screensaver askForPasswordDelay -int 0
          launchctl asuser "$primary_uid" sudo --user=${primaryUser} -- defaults -currentHost write com.apple.screensaver idleTime -int 0
        else
          echo >&2 "skipping current-host screensaver defaults; ${primaryUser} has no GUI launchd session"
        fi
      '';

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
        inputs.t3code-integration.overlays.client
        (final: prev: let
          previousUnwrapped = prev.t3code.unwrapped;
          unwrapped = previousUnwrapped.overrideAttrs (_finalAttrs: previousAttrs: {
            pnpmDeps = final.lib.overrideDerivation previousAttrs.pnpmDeps (_: {
              outputHash = "sha256-VdE+ycyF/UvPr2urLlKOt/Aa5t2WqKcidr2D0sH0eVI=";
            });
          });
        in {
          t3code = prev.t3code.overrideAttrs (previousAttrs: {
            paths = [unwrapped];
            passthru = (previousAttrs.passthru or {}) // {inherit unwrapped;};
          });
        })
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
      environment.systemPackages =
        essentialPkgs
        ++ [
          pkgs.gnupg
          paseoPackage
          paseoDesktopPackage
        ];

      nixpkgs.config.allowUnfree = true;

      # Install GUI-visible fonts into /Library/Fonts/Nix Fonts.
      fonts.packages = with pkgs; [
        nerd-fonts.jetbrains-mono
      ];

      # Desktop apps are managed separately from the Codex and Claude Code CLIs.
      homebrew = {
        enable = true;
        taps = builtins.attrNames config.nix-homebrew.taps;
        brews = [
          "ddcctl"
          "m1ddc"
        ];
        casks = [
          "claude"
          "chatgpt"
          "ghostty"
          "hammerspoon"
          "raycast"
          "spotify"
          "vlc"
        ];
        greedyCasks = true;
        onActivation = {
          cleanup = "zap";
          # Homebrew requires an explicit confirmation flag when
          # `brew bundle install` runs with `--cleanup`.
          extraFlags = ["--force"];
        };
      };

      # Auto upgrade nix package and the daemon service.
      launchd.user = lib.mkIf (primaryUser == activePrimaryUser) {
        envVariables.PATH = config.environment.systemPath;
      };

      programs.direnv.enable = true;

      # Necessary for using flakes on this system.
      nix.settings = {
        experimental-features = "nix-command flakes";
        # Trigger store GC before Nix consumes the machine's build headroom.
        min-free = 10 * 1024 * 1024 * 1024;
        max-free = 20 * 1024 * 1024 * 1024;
        gc-reserved-space = 256 * 1024 * 1024;
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
      nix.gc = {
        automatic = true;
        interval = {
          Hour = 4;
          Minute = 15;
        };
        options = "--delete-older-than 30d";
      };
      nix.optimise.automatic = true;

      # Set Git commit hash for darwin-version.
      system.configurationRevision = self.rev or self.dirtyRev or null;

      # Used for backwards compatibility, please read the changelog before changing
      system.stateVersion = 4;

      # The platform the configuration will be used on.

      nixpkgs.hostPlatform = "aarch64-darwin";
      users.users =
        lib.genAttrs personalUsers (user: {
          name = user;
          home = homeForUser user;
          description = mkUserDescription user;
          openssh.authorizedKeys.keys = inputs.railbird-secrets.keys.kanivanKeys;
        })
        // {
          gitea-runner = {
            name = "gitea-runner";
            isHidden = false;
            home = "/Users/gitea-runner";
            createHome = false;
          };
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
        sharedModules = sharedHomeModules;
        users = lib.genAttrs enabledHomeUsers (_: {});
      };
    };
    mkDarwinSystem = {
      primaryUser,
      enabledHomeUsers ? [primaryUser],
    }:
      nix-darwin.lib.darwinSystem {
        modules = [
          agenix.darwinModules.default
          home-manager.darwinModules.home-manager
          nix-homebrew.darwinModules.nix-homebrew
          {
            nix-homebrew = {
              enable = true;
              user = primaryUser;
              autoMigrate = true;
              package =
                inputs.brew-src
                // {
                  name = "brew-6.0.11";
                  version = "6.0.11";
                };
              taps = {
                "homebrew/homebrew-core" = inputs.homebrew-core;
                "homebrew/homebrew-cask" = inputs.homebrew-cask;
              };
            };
          }
          (mkConfiguration {inherit primaryUser enabledHomeUsers;})
        ];
      };
  in {
    # The default remains on the currently existing macOS account. Switch to
    # mac-demarco-mini-imalison after the target login account and home are in
    # place.
    darwinConfigurations."mac-demarco-mini" = mkDarwinSystem {
      primaryUser = activePrimaryUser;
      enabledHomeUsers = [activePrimaryUser];
    };

    darwinConfigurations."mac-demarco-mini-imalison" = mkDarwinSystem {
      primaryUser = targetPrimaryUser;
      enabledHomeUsers = [targetPrimaryUser];
    };

    # Expose the package set, including overlays, for convenience.
    darwinPackages = self.darwinConfigurations."mac-demarco-mini".pkgs;
  };
}
