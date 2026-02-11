{ config, inputs, pkgs, makeEnable, ... }:
makeEnable config "myModules.sni" true {
  home-manager.sharedModules = [
    {
      systemd.user.services.kanshi-sni = {
        Unit = {
          Description = "kanshi-sni tray app";
          After = [ "graphical-session.target" "tray.target" ];
          PartOf = [ "graphical-session.target" ];
          Requires = [ "tray.target" ];
        };
        Service = {
          ExecStart = "${inputs.kanshi-sni.packages.${pkgs.stdenv.hostPlatform.system}.default}/bin/kanshi-sni";
          Restart = "always";
          RestartSec = 3;
        };
        Install = {
          WantedBy = [ "graphical-session.target" ];
        };
      };

      services.blueman-applet.enable = true;

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

      services.flameshot.enable = true;
    }
  ];
}
