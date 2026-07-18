{
  config,
  inputs,
  lib,
  pkgs,
  makeEnable,
  ...
}: let
  system = pkgs.stdenv.hostPlatform.system;
  kanshiSniPackage = inputs.kanshi-sni.packages.${system}.default;
in
  makeEnable config "myModules.sni" true {
    systemd.user.services.blueman-applet.serviceConfig.ExecStart = lib.mkForce [
      ""
      "${pkgs.blueman}/bin/blueman-applet"
    ];

    home-manager.sharedModules = [
      ({lib, ...}: {
        systemd.user.services.kanshi-sni = {
          Unit = {
            Description = "kanshi-sni tray app";
            After = ["graphical-session.target" "tray.target" "kanshi.service"];
            PartOf = ["graphical-session.target" "kanshi.service"];
            Requires = ["tray.target"];
            Wants = ["kanshi.service"];
          };
          Service = {
            ExecStart = "${kanshiSniPackage}/bin/kanshi-sni";
            Restart = "always";
            RestartSec = 3;
          };
          Install = {
            WantedBy = ["graphical-session.target"];
          };
        };

        systemd.user.services.localsend = {
          Unit = {
            Description = "LocalSend tray app";
            After = ["graphical-session.target" "tray.target"];
            PartOf = ["graphical-session.target"];
            Requires = ["tray.target"];
          };
          Service = {
            ExecStart = "${pkgs.localsend}/bin/localsend_app --hidden";
            Restart = "on-failure";
            RestartSec = 3;
          };
          Install = {
            WantedBy = ["graphical-session.target"];
          };
        };

        services.kdeconnect = {
          enable = true;
          indicator = true;
        };

        home.activation.disableKdeConnectBluetooth = lib.hm.dag.entryAfter ["writeBoundary"] ''
          ${pkgs.kdePackages.kconfig}/bin/kwriteconfig6 \
            --file kdeconnect/config \
            --group General \
            --key disabled_providers \
            'BluetoothLinkProvider,AsyncLinkProvider'
        '';

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
      })
    ];
  }
