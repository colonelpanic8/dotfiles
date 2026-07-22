{
  config,
  lib,
  makeEnable,
  pkgs,
  ...
}: let
  cfg = config.myModules.t3codeServer;
  # T3 hydrates PATH from the login shell, but the service still needs a
  # deterministic bootstrap PATH for Tailscale and common provider tools.
  servicePath = lib.makeBinPath (with pkgs; [
    bashInteractive
    claude-code
    codex
    coreutils
    findutils
    gh
    git
    gnugrep
    gnused
    nix
    nodejs
    openssh
    ripgrep
    t3code
    tailscale
    zsh
  ]);
  enabledModule = makeEnable config "myModules.t3codeServer" false {
    home-manager.users.imalison.systemd.user.services.t3code-headless = {
      Unit = {
        Description = "T3 Code headless server over Tailscale";
        Documentation = "https://github.com/pingdotgg/t3code/blob/main/REMOTE.md";
        After = [cfg.startTarget];
        ConditionPathIsDirectory = "/srv/dotfiles";
        StartLimitIntervalSec = 300;
        StartLimitBurst = 5;
      };

      Service = {
        Type = "simple";
        WorkingDirectory = "/srv/dotfiles";
        Environment = [
          "PATH=${servicePath}:/run/current-system/sw/bin"
          # Keep this independent from the Electron-owned backend and make
          # `t3 project`/`t3 auth` use the same state by default in a shell.
          "T3CODE_HOME=/home/imalison/.t3"
        ];
        # The desktop app currently uses 3773. Keep the persistent backend on a
        # stable loopback-only port and let Tailscale Serve expose it over HTTPS.
        ExecStart = "${pkgs.t3code}/bin/t3 serve --host 127.0.0.1 --port 3774 --tailscale-serve --tailscale-serve-port ${toString cfg.tailscaleServePort} /srv/dotfiles";
        Restart = "always";
        RestartSec = 5;
        TimeoutStopSec = 15;
        UMask = "0077";
      };

      # Desktop hosts start after their graphical environment is imported so
      # provider/plugin children receive Wayland, X11, and D-Bus access. Servers
      # can select default.target instead. Deliberately omit PartOf so the backend
      # is not stopped when a graphical session ends.
      Install.WantedBy = [cfg.startTarget];
    };
  };
in
  enabledModule
  // {
    options = lib.recursiveUpdate enabledModule.options {
      myModules.t3codeServer = {
        tailscaleServePort = lib.mkOption {
          type = lib.types.port;
          default = 443;
          description = "Tailnet-only HTTPS port exposed by Tailscale Serve.";
        };

        startTarget = lib.mkOption {
          type = lib.types.str;
          default = "graphical-session.target";
          description = "User systemd target that starts the headless T3 Code service.";
        };
      };
    };
  }
