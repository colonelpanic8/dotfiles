{ config, makeEnable, ... }:
makeEnable config "myModules.sni" true {
  home-manager.sharedModules = [
    {
      services.blueman-applet = {
        enable = true;
      };

      services.kdeconnect = {
        enable = true;
        indicator = true;
      };

      services.network-manager-applet.enable = true;

      # Disable the XDG autostart for nm-applet since we're managing it via systemd.
      # The XDG autostart races with the systemd service and doesn't use --indicator.
      xdg.configFile."autostart/nm-applet.desktop".text = ''
        [Desktop Entry]
        Hidden=true
      '';

      services.udiskie = {
        enable = true;
        tray = "always";
      };

      services.status-notifier-watcher = {
        enable = true;
        flags = ["--log-level" "DEBUG"];
      };

      services.pasystray.enable = true;

      services.flameshot = {
        enable = true;
      };
    }
  ];
}
