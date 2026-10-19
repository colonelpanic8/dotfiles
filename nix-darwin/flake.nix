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
    paseoConfiguration = {
      user,
      passwordSecret,
      hostnames ? null,
      disableRelay ? true,
    }: {
      config,
      lib,
      pkgs,
      ...
    }: let
      paseoHome = "${homeForUser user}/.paseo";
      paseoPackage = inputs.paseo.packages.${pkgs.stdenv.hostPlatform.system}.default.overrideAttrs (old: {
        postInstall =
          (old.postInstall or "")
          + ''
            # Runtime tracing misses node-pty's dynamically loaded native files.
            for path in {node_modules,packages/server/node_modules}/node-pty/{build/Release,prebuilds/darwin-arm64}/{pty.node,spawn-helper}; do
              [ -f "$path" ] || continue
              mkdir -p "$out/lib/paseo/$(dirname "$path")"
              cp -p "$path" "$out/lib/paseo/$path"
            done
            node -e "require('$out/lib/paseo/packages/server/node_modules/node-pty')"
          '';
      });
      paseoDesktopPackage = inputs.paseo.packages.${pkgs.stdenv.hostPlatform.system}.desktop.override {
        paseo = paseoPackage;
      };
      ensurePaseoMcpInjection = import ../nix-shared/ensure-paseo-mcp-injection.nix {inherit pkgs;};
      # Tailscale.app's CLI cannot run headless, so without nix-darwin's
      # tailscale service read the address off the interface instead.
      tailscaleIpCommand =
        if config.services.tailscale.enable
        then "${config.services.tailscale.package}/bin/tailscale ip -4 2>/dev/null | ${pkgs.coreutils}/bin/head -n 1"
        else ''/sbin/ifconfig | /usr/bin/awk '/inet 100\./ {print $2; exit}' '';
      paseoDaemon = pkgs.writeShellScript "paseo-daemon" ''
        set -eu

        ${ensurePaseoMcpInjection} ${lib.escapeShellArg "${paseoHome}/config.json"}

        secret_file='${config.age.secrets.${passwordSecret}.path}'
        # wait4path only rechecks on mount events, so it misses secrets agenix
        # creates after this starts.
        attempts=0
        while [ ! -r "$secret_file" ]; do
          if [ "$attempts" -ge 60 ]; then
            echo "Timed out waiting for $secret_file" >&2
            exit 1
          fi
          attempts=$((attempts + 1))
          sleep 1
        done
        # Accept either an environment file or a bare password.
        password_line="$(${pkgs.gnugrep}/bin/grep -m1 '^PASEO_PASSWORD=' "$secret_file" || ${pkgs.coreutils}/bin/head -n 1 "$secret_file")"
        password="''${password_line#PASEO_PASSWORD=}"
        if [ -z "$password" ]; then
          echo "Paseo password secret is empty" >&2
          exit 1
        fi

        tailscale_ip=""
        attempts=0
        while [ "$attempts" -lt 30 ]; do
          tailscale_ip="$(${tailscaleIpCommand} || true)"
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
        exec ${paseoPackage}/bin/paseo-server ${lib.optionalString disableRelay "--no-relay"}
      '';
    in {
      age.secrets.${passwordSecret} = {
        file = ../nixos/secrets + "/${passwordSecret}.age";
        owner = user;
        mode = "0400";
      };

      environment.systemPackages = [
        paseoPackage
        paseoDesktopPackage
      ];

      home-manager.users.${user} = {lib, ...}: {
        home.activation.configurePaseoDesktopDaemon = lib.hm.dag.entryAfter ["writeBoundary"] ''
          settings_dir="$HOME/Library/Application Support/Paseo"
          settings_file="$settings_dir/desktop-settings.json"
          mkdir -p "$settings_dir"
          temporary="$(${pkgs.coreutils}/bin/mktemp "$settings_dir/.desktop-settings.json.XXXXXX")"
          if [ -f "$settings_file" ]; then
            ${pkgs.jq}/bin/jq '. * {settings: {daemon: {manageBuiltInDaemon: false, keepRunningAfterQuit: false}}, migrations: {legacyRendererSettingsImported: true}}' \
              "$settings_file" > "$temporary"
          else
            printf '%s\n' '{"version":1,"settings":{"daemon":{"manageBuiltInDaemon":false,"keepRunningAfterQuit":false}},"migrations":{"legacyRendererSettingsImported":true}}' > "$temporary"
          fi
          chmod 0600 "$temporary"
          mv "$temporary" "$settings_file"
        '';
      };

      launchd.daemons.paseo = {
        serviceConfig = {
          ProgramArguments = ["${paseoDaemon}"];
          UserName = user;
          GroupName = "staff";
          WorkingDirectory = homeForUser user;
          EnvironmentVariables = {
            HOME = homeForUser user;
            USER = user;
            LOGNAME = user;
            SHELL = "/bin/zsh";
            NODE_ENV = "production";
            PASEO_HOME = paseoHome;
            PASEO_HOSTNAMES = lib.concatStringsSep "," (
              if hostnames == null
              then [config.networking.hostName]
              else hostnames
            );
            PATH = lib.concatStringsSep ":" [
              "/etc/profiles/per-user/${user}/bin"
              "${homeForUser user}/.nix-profile/bin"
              "${homeForUser user}/.local/state/nix/profile/bin"
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
          StandardOutPath = "${homeForUser user}/Library/Logs/paseo-daemon.log";
          StandardErrorPath = "${homeForUser user}/Library/Logs/paseo-daemon.err.log";
        };
      };
    };
    # Runs the Home Manager T3 Code server as a system daemon, like Paseo, so it
    # does not depend on the user having a GUI login session.
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
    desktopConfiguration = {primaryUser}: {
      pkgs,
      lib,
      config,
      ...
    }: let
      essentialPkgs = (import ../nix-shared/system/essential.nix {inherit pkgs lib inputs;}).environment.systemPackages;
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

      environment.systemPackages =
        essentialPkgs
        ++ [pkgs.gnupg];

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
      paseoPasswordSecret ? "paseo-password-environment",
      t3codeTokenSecret ? "t3code-managed-access-token",
    }:
      nix-darwin.lib.darwinSystem {
        modules = [
          agenix.darwinModules.default
          home-manager.darwinModules.home-manager
          nix-homebrew.darwinModules.nix-homebrew
          (homebrewConfiguration {inherit primaryUser;})
          (sharedConfiguration {inherit primaryUser manageNix;})
          (paseoConfiguration {
            user = primaryUser;
            passwordSecret = paseoPasswordSecret;
          })
          (t3codeConfiguration {
            user = primaryUser;
            tokenSecret = t3codeTokenSecret;
          })
          (homeManagerConfiguration {
            inherit primaryUser;
            users = nixpkgs.lib.genAttrs enabledHomeUsers (_: homeModules);
          })
          (desktopConfiguration {inherit primaryUser;})
          machineModule
        ];
      };
  in {
    darwinConfigurations."kats-macbook-neo" = mkDarwinSystem {
      primaryUser = "kat";
      machineModule = ./machines/kats-macbook-neo.nix;
      manageNix = false;
      homeModules = [./home/agents.nix ./home/desktop.nix];
      paseoPasswordSecret = "paseo-password-kats-macbook-neo";
      t3codeTokenSecret = "t3code-managed-access-token.kats-macbook-neo";
    };

    darwinConfigurations."mac-demarco-mini" = mkDarwinSystem {
      primaryUser = "imalison";
      enabledHomeUsers = personalUsers;
      machineModule = ./machines/mac-demarco-mini.nix;
    };

    darwinConfigurations."alexanders-macbook-air" = nix-darwin.lib.darwinSystem {
      modules = [
        agenix.darwinModules.default
        home-manager.darwinModules.home-manager
        (sharedConfiguration {primaryUser = "alex";})
        (import ./tailscale.nix {
          authKeyFile = ../nixos/secrets/tailscale-authkey.alexanders-macbook-air.age;
        })
        (paseoConfiguration {
          user = "alex";
          passwordSecret = "paseo-password-alexanders-macbook-air";
          disableRelay = false;
          hostnames = [
            "alexanders-macbook-air"
            "alexanders-macbook-air.tail40f15e.ts.net"
          ];
        })
        (homeManagerConfiguration {
          primaryUser = "alex";
          users.alex = [
            {
              dotfiles.gitIdentity = {
                name = "Alexander Malison";
                email = "alexmalison@gmail.com";
              };
            }
          ];
        })
        nix-homebrew.darwinModules.nix-homebrew
        (homebrewConfiguration {
          primaryUser = "alex";
          extraCasks = ["google-chrome"];
          cleanup = "none";
        })
        ({
          pkgs,
          lib,
          ...
        }: {
          networking.hostName = "alexanders-macbook-air";
          system.stateVersion = 7;
          environment.systemPackages =
            (import ../nix-shared/system/essential.nix {inherit pkgs lib inputs;}).environment.systemPackages
            ++ (with pkgs; [
              bitwarden-desktop
              gnupg
            ]);
          users.users.alex = {
            name = "alex";
            home = homeForUser "alex";
            openssh.authorizedKeys.keys = (import ../nix-shared/keys.nix).userKeys.alex;
          };
        })
      ];
    };

    # Expose the package set, including overlays, for convenience.
    darwinPackages = self.darwinConfigurations."mac-demarco-mini".pkgs;
  };
}
