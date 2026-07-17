{
  config,
  lib,
  pkgs,
  inputs,
  makeEnable,
  ...
}: let
  cfg = config.myModules."keepbook-sync";
  keepbookPackages = inputs.keepbook.packages.${pkgs.stdenv.hostPlatform.system};
  keepbookDioxusDesktopBase = keepbookPackages.keepbook-dioxus-desktop;
  keepbookDioxusDesktop = pkgs.symlinkJoin {
    name = "${keepbookDioxusDesktopBase.name}-single-desktop-entry";
    paths = [keepbookDioxusDesktopBase];
    postBuild = ''
      desktopFile="$out/share/applications/keepbook-dioxus.desktop"
      if [ -f "$desktopFile" ] && ! grep -q '^NoDisplay=' "$desktopFile"; then
        tmpDesktopFile="$(mktemp)"
        cp "$desktopFile" "$tmpDesktopFile"
        rm -f "$desktopFile"
        install -m644 "$tmpDesktopFile" "$desktopFile"
        rm -f "$tmpDesktopFile"
        echo 'NoDisplay=true' >> "$desktopFile"
      fi
    '';
    meta = keepbookDioxusDesktopBase.meta;
  };
  keepbookDioxusExec = "${keepbookDioxusDesktop}/bin/keepbook-dioxus";

  enabledModule = makeEnable config "myModules.keepbook-sync" false {
    environment.systemPackages = [keepbookDioxusDesktop];

    home-manager.users.${cfg.user} = {
      systemd.user.services.keepbook-dioxus = {
        Unit = {
          Description = "Keepbook Dioxus desktop app";
          After = ["graphical-session.target" "tray.target" "xsettingsd.service"];
          PartOf = ["graphical-session.target"];
          Requires = ["tray.target"];
        };
        Service = {
          ExecStart = keepbookDioxusExec;
          WorkingDirectory = builtins.dirOf cfg.configPath;
          Restart = "on-failure";
          RestartSec = 5;
          Environment = [
            "RUST_LOG=info"
            "KEEPBOOK_START_MINIMIZED_TO_TRAY=${lib.boolToString cfg.startMinimizedToTray}"
          ];
        };
        Install = {
          WantedBy = ["graphical-session.target"];
        };
      };
    };
  };
in
  enabledModule
  // {
    # Merge our extra options with the enable option produced by makeEnable.
    options = lib.recursiveUpdate enabledModule.options {
      myModules."keepbook-sync" = {
        user = lib.mkOption {
          type = lib.types.str;
          default = "imalison";
          description = "User account to run the keepbook Dioxus desktop app.";
        };

        configPath = lib.mkOption {
          type = lib.types.str;
          default = "/home/imalison/.local/share/keepbook/keepbook.toml";
          description = "Path to keepbook.toml. The Dioxus app is launched from this file's directory so keepbook's default config discovery finds it.";
        };

        startMinimizedToTray = lib.mkOption {
          type = lib.types.bool;
          default = true;
          description = "Start the Keepbook desktop app hidden and access it from the tray.";
        };
      };
    };
  }
