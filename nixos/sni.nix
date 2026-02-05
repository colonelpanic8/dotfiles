{ config, lib, makeEnable, ... }:
makeEnable config "myModules.sni" true {
  home-manager.sharedModules = [
    {
      systemd.user.services =
        let
          wantGraphicalPre = {
            Install.WantedBy = lib.mkAfter [ "graphical-session-pre.target" ];
          };
        in
        {
          blueman-applet = wantGraphicalPre;
          kdeconnect = wantGraphicalPre;
          kdeconnect-indicator = wantGraphicalPre;
          network-manager-applet = wantGraphicalPre;
          pasystray = wantGraphicalPre;
          udiskie = wantGraphicalPre;
          flameshot = wantGraphicalPre;
        };

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

      services.pasystray.enable = true;

      services.flameshot = {
        enable = true;
      };
    }
  ];
}
