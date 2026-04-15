{ config, inputs, pkgs, makeEnable, ... }:
let
  system = pkgs.stdenv.hostPlatform.system;
  kanshiSniPackage =
    inputs.kanshi-sni.packages.${system}.default.overrideAttrs (old: {
      patches = (old.patches or [ ]) ++ [
        ./patches/kanshi-sni-refresh-and-reconnect.patch
      ];
    });
in
makeEnable config "myModules.sni" true {
  home-manager.sharedModules = [
    {
      systemd.user.services.kanshi-sni = {
        Unit = {
          Description = "kanshi-sni tray app";
          After = [ "graphical-session.target" "tray.target" "kanshi.service" ];
          PartOf = [ "graphical-session.target" "kanshi.service" ];
          Requires = [ "tray.target" ];
          Wants = [ "kanshi.service" ];
        };
        Service = {
          ExecStart = "${kanshiSniPackage}/bin/kanshi-sni";
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

      services.flameshot = {
        enable = true;
        settings = {
          General = {
            useGrimAdapter = true;
          };
        };
      };
    }
  ];
}
