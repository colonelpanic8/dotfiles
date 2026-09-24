{
  config,
  lib,
  pkgs,
  primaryUser,
  ...
}: let
  settingsSeed = {
    version = 1;

    app.keyboardShortcutOverrides =
      (import ../paseo-favorites.nix).keyboardShortcutOverrides
      // {
        "command-center.shortcut:thinking:low" = "F17";
        "command-center.shortcut:thinking:medium" = "F18";
        "command-center.shortcut:thinking:high" = "F19";
      };

    desktop = {
      releaseChannel = "stable";
      daemon = {
        manageBuiltInDaemon = pkgs.stdenv.isLinux;
        keepRunningAfterQuit = false;
      };
    };
  };

  settingsSeedJson = builtins.toJSON settingsSeed;
in
  lib.mkIf (config.home.username == primaryUser) (lib.mkMerge [
    (lib.mkIf pkgs.stdenv.isLinux {
      xdg.configFile."paseo/settings-seed.json".text = settingsSeedJson;
    })

    (lib.mkIf pkgs.stdenv.isDarwin {
      home.file."Library/Application Support/Paseo/settings-seed.json".text = settingsSeedJson;
    })
  ])
