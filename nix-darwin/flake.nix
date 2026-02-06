{
  description = "Example Darwin system flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nix-darwin.url = "github:LnL7/nix-darwin";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
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

    codex-cli-nix = {
      # Default branch is `main` on GitHub (not `master`).
      url = "github:sadjow/codex-cli-nix/main";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    claude-code-nix = {
      url = "github:sadjow/claude-code-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ self, nix-darwin, nixpkgs, home-manager, ... }:
  let
    libDir = ../dotfiles/lib;
    configuration = { pkgs, lib, config, ... }:
    let
      essentialPkgs = (import ../nixos/essential.nix { inherit pkgs lib; }).environment.systemPackages;
    in {
      networking.hostName = "mac-demarco-mini";
      imports = [ (import ./gitea-actions-runner.nix) ];
      services.gitea-actions-runner = {
        user = "gitea-runner";
        instances.nix = {
          enable = true;
          name = config.networking.hostName;
          url = "https://dev.railbird.ai";
          token = "H0A7YXAWsKSp9QzvMymfJI12hbxwR7UerEHpCJUe";
          labels = [
            "nix-darwin-${pkgs.system}:host"
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
        (import ../nixos/overlay.nix)
        # Use codex and claude-code from dedicated flakes with cachix
        (final: prev: {
          codex = inputs.codex-cli-nix.packages.${prev.stdenv.hostPlatform.system}.default;
          claude-code = inputs.claude-code-nix.packages.${prev.stdenv.hostPlatform.system}.default;
        })
      ];
      environment.systemPackages =
        essentialPkgs
        ++ (with pkgs; [
          alejandra
          claude-code
          cocoapods
          codex
          nodePackages.prettier
          nodejs
          slack
          tea
          typescript
          vim
          yarn
        ]);

      nixpkgs.config.allowUnfree = true;


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

      home-manager.useGlobalPkgs = true;      home-manager.useUserPackages = true;

      users.users.kat = {
        name = "kat";
        home = "/Users/kat";
      };

      programs.zsh = {
        enable = true;
        shellInit = ''
          fpath+="${libDir}/functions"
          for file in "${libDir}/functions/"*
          do
          autoload "''${file##*/}"
          done
        '';
        interactiveShellInit = ''
          # eval "$(register-python-argcomplete prb)"
          # eval "$(register-python-argcomplete prod-prb)"
          # eval "$(register-python-argcomplete railbird)"
          # [ -n "$EAT_SHELL_INTEGRATION_DIR" ] && source "$EAT_SHELL_INTEGRATION_DIR/zsh"

          autoload -Uz bracketed-paste-magic
          zle -N bracketed-paste bracketed-paste-magic
        '';
      };

      home-manager.users.kat = {
        programs.starship = {
          enable = true;
        };
        programs.zsh.enable = true;
        home.stateVersion = "24.05";
      };
    };
  in
  {
    darwinConfigurations."mac-demarco-mini" = nix-darwin.lib.darwinSystem {
      modules = [
        home-manager.darwinModules.home-manager
        configuration
      ];
    };

    # Expose the package set, including overlays, for convenience.
    darwinPackages = self.darwinConfigurations."mac-demarco-mini".pkgs;
  };
}
