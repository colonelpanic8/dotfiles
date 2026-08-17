{
  config,
  lib,
  pkgs,
  ...
}: let
  settingsSeed = {
    version = 1;

    app.keyboardShortcutOverrides = {
      "command-center.shortcut:models:claude:claude-fable-5" = "F13";
      "command-center.shortcut:models:claude:claude-opus-5" = "F14";
      "command-center.shortcut:models:codex:gpt-5.6-sol" = "F15";
      "command-center.shortcut:models:codex:gpt-5.6-luna" = "F16";
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
  lib.mkIf (config.home.username == "imalison") (lib.mkMerge [
    (lib.mkIf pkgs.stdenv.isLinux {
      xdg.configFile."paseo/settings-seed.json".text = settingsSeedJson;
    })

    (lib.mkIf pkgs.stdenv.isDarwin {
      home.file."Library/Application Support/Paseo/settings-seed.json".text = settingsSeedJson;
    })
  ])
