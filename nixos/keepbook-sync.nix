{ config, lib, pkgs, inputs, makeEnable, ... }:
let
  cfg = config.myModules."keepbook-sync";
  keepbookTray = inputs.keepbook.packages.${pkgs.stdenv.hostPlatform.system}.keepbook-tray;

  daemonArgs = [
    "--config" cfg.configPath
    "--interval" cfg.interval
    "--jitter" cfg.jitter
    "--history-points" (toString cfg.historyPoints)
    "--tray-icon" cfg.trayIcon
  ]
  ++ lib.optionals (!cfg.syncOnStart) [ "--no-sync-on-start" ]
  ++ lib.optionals (!cfg.syncPrices) [ "--no-sync-prices" ]
  ++ lib.optionals (!cfg.syncSymlinks) [ "--no-sync-symlinks" ]
  ++ lib.optionals (cfg.balanceStaleness != null) [ "--balance-staleness" cfg.balanceStaleness ]
  ++ lib.optionals (cfg.priceStaleness != null) [ "--price-staleness" cfg.priceStaleness ];

  daemonExec = lib.escapeShellArgs ([ "${keepbookTray}/bin/keepbook-sync-daemon" ] ++ daemonArgs);

  enabledModule = makeEnable config "myModules.keepbook-sync" false {
    home-manager.users.${cfg.user} = {
      systemd.user.services.keepbook-sync-daemon = {
        Unit = {
          Description = "keepbook sync daemon";
          After = [ "graphical-session.target" "tray.target" ];
          PartOf = [ "graphical-session.target" ];
          Requires = [ "tray.target" ];
        };
        Service = {
          ExecStart = daemonExec;
          Restart = "always";
          RestartSec = 5;
          Environment = [ "RUST_LOG=info" ];
        };
        Install = {
          WantedBy = [ "graphical-session.target" ];
        };
      };
    };
  };
in
enabledModule // {
  # Merge our extra options with the enable option produced by makeEnable.
  options = lib.recursiveUpdate enabledModule.options {
    myModules."keepbook-sync" = {
      user = lib.mkOption {
        type = lib.types.str;
        default = "imalison";
        description = "User account to run the keepbook sync daemon.";
      };

      configPath = lib.mkOption {
        type = lib.types.str;
        default = "/home/imalison/.local/share/keepbook/keepbook.toml";
        description = "Path to keepbook.toml used by the daemon.";
      };

      interval = lib.mkOption {
        type = lib.types.str;
        default = "30m";
        description = "Base sync interval for keepbook-sync-daemon (e.g. 30m, 1h).";
      };

      jitter = lib.mkOption {
        type = lib.types.str;
        default = "5m";
        description = "Random jitter applied around each scheduled interval.";
      };

      balanceStaleness = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "Optional override for balance staleness threshold.";
      };

      priceStaleness = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "Optional override for price staleness threshold.";
      };

      syncOnStart = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Run a sync cycle immediately when the daemon starts.";
      };

      syncPrices = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Enable periodic price refresh during sync cycles.";
      };

      syncSymlinks = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Enable periodic symlink rebuild during sync cycles.";
      };

      historyPoints = lib.mkOption {
        type = lib.types.int;
        default = 8;
        description = "Recent portfolio history rows shown in tray menu.";
      };

      trayIcon = lib.mkOption {
        type = lib.types.str;
        default = "wallet";
        description = "Freedesktop icon name for idle keepbook tray state.";
      };
    };
  };
}
