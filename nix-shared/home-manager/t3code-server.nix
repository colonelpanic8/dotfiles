{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.myModules.t3codeServer;
  serverCommand = import ../t3code-server.nix {
    inherit lib pkgs;
    homeDirectory = config.home.homeDirectory;
    inherit (cfg) localPort repositoryRoot tailscaleServePort;
  };
in {
  options.myModules.t3codeServer = {
    enable = lib.mkEnableOption "the persistent T3 Code headless server";

    repositoryRoot = lib.mkOption {
      type = lib.types.str;
      default = "${config.home.homeDirectory}/dotfiles";
      description = "Repository opened by the persistent T3 Code server.";
    };

    localPort = lib.mkOption {
      type = lib.types.port;
      default = 3774;
      description = "Loopback port used by the persistent T3 Code server.";
    };

    tailscaleServePort = lib.mkOption {
      type = lib.types.port;
      default = 443;
      description = "Tailnet-only HTTPS port exposed by Tailscale Serve.";
    };

    systemdStartTarget = lib.mkOption {
      type = lib.types.str;
      default = "graphical-session.target";
      description = "User systemd target that starts the headless T3 Code service.";
    };
  };

  config = lib.mkIf cfg.enable (lib.mkMerge [
    {
      home.packages = [pkgs.t3code];
    }

    (lib.mkIf pkgs.stdenv.isLinux {
      systemd.user.services.t3code-headless = {
        Unit = {
          Description = "T3 Code headless server over Tailscale";
          Documentation = "https://github.com/pingdotgg/t3code/blob/main/REMOTE.md";
          After = [cfg.systemdStartTarget];
          ConditionPathIsDirectory = cfg.repositoryRoot;
          StartLimitIntervalSec = 300;
          StartLimitBurst = 5;
        };

        Service = {
          Type = "simple";
          ExecStart = "${serverCommand}";
          Restart = "always";
          RestartSec = 5;
          TimeoutStopSec = 15;
          UMask = "0077";
        };

        # Desktop hosts start after their graphical environment is imported so
        # provider/plugin children receive Wayland, X11, and D-Bus access.
        # Servers can select default.target instead. Deliberately omit PartOf so
        # the backend is not stopped when a graphical session ends.
        Install.WantedBy = [cfg.systemdStartTarget];
      };
    })

    (lib.mkIf pkgs.stdenv.isDarwin {
      launchd.agents.t3code-headless = {
        enable = true;
        config = {
          ProgramArguments = ["${serverCommand}"];
          KeepAlive = true;
          ProcessType = "Interactive";
          RunAtLoad = true;
          ThrottleInterval = 5;
          StandardOutPath = "${config.home.homeDirectory}/Library/Logs/t3code-headless.log";
          StandardErrorPath = "${config.home.homeDirectory}/Library/Logs/t3code-headless.err.log";
        };
      };
    })
  ]);
}
