{
  config,
  lib,
  pkgs,
  ...
}: let
  guiApplicationLauncher = application:
    pkgs.writeShellScript "launch-${lib.toLower application}-with-aqua" ''
      while true; do
        while ! /bin/launchctl print "gui/$UID" >/dev/null 2>&1; do
          /bin/sleep 5
        done

        /usr/bin/open -gja ${lib.escapeShellArg application} || true

        while /bin/launchctl print "gui/$UID" >/dev/null 2>&1; do
          /bin/sleep 5
        done
      done
    '';
in {
  launchd.agents.hammerspoon = {
    enable = true;
    domain = "user";
    config = {
      ProgramArguments = ["${guiApplicationLauncher "Hammerspoon"}"];
      ProcessType = "Background";
      RunAtLoad = true;
    };
  };

  launchd.agents.raycast = {
    enable = true;
    domain = "user";
    config = {
      ProgramArguments = ["${guiApplicationLauncher "Raycast"}"];
      ProcessType = "Background";
      RunAtLoad = true;
      StandardOutPath = "${config.home.homeDirectory}/Library/Logs/raycast-launchd.log";
      StandardErrorPath = "${config.home.homeDirectory}/Library/Logs/raycast-launchd.err.log";
    };
  };

  home.activation.configureRaycastHotkey = lib.hm.dag.entryAfter ["writeBoundary"] ''
    raycast_domain="com.raycast.macos"
    desired_hotkey="Command-49"
    current_hotkey="$(/usr/bin/defaults read "$raycast_domain" raycastGlobalHotkey 2>/dev/null || true)"

    if [ -d /Applications/Raycast.app ]; then
      /usr/bin/xattr -dr com.apple.quarantine /Applications/Raycast.app 2>/dev/null || true
    fi

    if [ "$current_hotkey" != "$desired_hotkey" ]; then
      /usr/bin/defaults write "$raycast_domain" raycastGlobalHotkey -string "$desired_hotkey"
      /usr/bin/defaults write "$raycast_domain" mainWindow_isMonitoringGlobalHotkeys -bool true

      if /usr/bin/pgrep -x Raycast >/dev/null 2>&1; then
        /usr/bin/killall Raycast || true
        /bin/sleep 1
      fi
      if /bin/launchctl print "gui/$UID" >/dev/null 2>&1; then
        /usr/bin/open /Applications/Raycast.app || true
      fi
    fi
  '';

  launchd.agents.alt-tab = lib.mkIf pkgs.stdenv.isDarwin {
    enable = false;
    config = {
      ProgramArguments = [
        "/usr/bin/open"
        "-gj"
        "${pkgs.alt-tab-macos}/Applications/AltTab.app"
      ];
      KeepAlive = false;
      ProcessType = "Interactive";
      RunAtLoad = true;
      StandardOutPath = "${config.home.homeDirectory}/Library/Logs/alt-tab-launchd.log";
      StandardErrorPath = "${config.home.homeDirectory}/Library/Logs/alt-tab-launchd.err.log";
    };
  };
}
