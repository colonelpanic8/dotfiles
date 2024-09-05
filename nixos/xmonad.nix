{ config, pkgs, inputs, forEachUser, makeEnable, ... }:
makeEnable config "modules.xmonad" true  {
  nixpkgs.overlays = with inputs; [
    xmonad.overlay
    xmonad-contrib.overlay
    notifications-tray-icon.overlay
    (import ../dotfiles/config/xmonad/overlay.nix)
  ] ++ taffybar.overlays;

  services.xserver = {
    windowManager = {
      session = [
        {
          name = "xmonad";
          start = ''
            /usr/bin/env imalison-xmonad &
            waitPID=$!
          '';
        }
      ];
    };
  };

  environment.systemPackages = with pkgs; [
    # Haskell Desktop
    haskellPackages.xmonad
    haskellPackages.imalison-xmonad
    haskellPackages.notifications-tray-icon
    haskellPackages.gtk-sni-tray
    haskellPackages.status-notifier-item
    haskellPackages.dbus-hslogger
    inputs.imalison-taffybar.defaultPackage."${pkgs.system}"
  ];

  home-manager.users = forEachUser {
    imports = [ ./dunst.nix ];

    services.blueman-applet = {
      enable = true;
    };

    services.taffybar = {
      enable = true;
      package = inputs.imalison-taffybar.defaultPackage."${pkgs.system}";
    };

    services.kdeconnect = {
      enable = true;
      indicator = true;
    };

    services.network-manager-applet.enable = true;

    services.udiskie = {
      enable = true;
      tray = "always";
    };

    services.status-notifier-watcher.enable = true;

    services.autorandr.enable = true;

    services.random-background = {
      enable = true;
      display = "fill";
      interval = "1h";
      imageDirectory = "/var/lib/syncthing/sync/Wallpaper/";
    };

    services.xsettingsd.enable = true;

    services.volnoti.enable = true;

    services.pasystray.enable = true;

    services.parcellite = {
      enable = true;
      package = pkgs.clipit;
    };

    services.picom = {
      enable = true;
      vSync = config.modules.xmonad.picom.vSync.enable;
      backend = "glx";
      # extraArgs = ["--experimental-backends"];
      package = pkgs.picom-pijulius;

      settings = {
        animations = true;
        animation-window-mass = 1;
        animation-dampening = 20;
        animation-stiffness = 250;
        animation-clamping = false;
        animation-for-open-window = "zoom";
        animation-for-unmap-window = "zoom";
        animation-for-transient-window = "slide-down";
        fade-in-step = 0.028;
        fade-out-step = 0.028;
        fading = true;
      };

      wintypes = {
        dock = {animation = "slide-down";};
        toolbar = {animation = "slide-down";};
      };

      settings = {
        focus-exclude = ["class_g ?= 'rofi'" "class_g ?= 'Steam'"];
        rounded-corners-exclude = [
          "! name~=''" # Qtile == empty wm_class..
          "window_type = 'dock'"
          "window_type = 'desktop'"
          "class_g ?= 'Dunst'"
        ];

        corner-radius = 10;
        round-borders = 0;
        round-borders-exclude = [
          "! name~=''" # Qtile == empty wm_class..
        ];

        daemon = false;
        dbus = false;
        mark-wmwin-focused = false;
        mark-ovredir-focused = false;
        detect-rounded-corners = true;
        detect-client-opacity = true;

        unredir-if-possible = false;
        unredir-if-possible-exclude = [];
        detect-transient = true;
        detect-client-leader = true;

        invert-color-include = [];
        glx-no-stencil = true;
        use-damage = false;
        transparent-clipping = false;
      };
    };

    systemd.user.services.notifications-tray-icon = {
      Unit = {
        Description = "Notifications tray icon";
        After = [ "graphical-session-pre.target" "tray.target" ];
        PartOf = [ "graphical-session.target" ];
      };

      Install = { WantedBy = [ "graphical-session.target" ]; };

      Service = {
        ExecStart = "${pkgs.haskellPackages.notifications-tray-icon}/bin/notifications-tray-icon  --github-token-pass dfinity-github-api-token";
        Restart = "always";
        RestartSec = 3;
      };
    };

    systemd.user.services.shutter = {
      Unit = {
        Description = "Shutter";
        After = [ "graphical-session-pre.target" "tray.target" ];
        PartOf = [ "graphical-session.target" ];
      };

      Install = { WantedBy = [ "graphical-session.target" ]; };

      Service = {
        ExecStart = "${pkgs.shutter}/bin/shutter --min_at_startup";
        Restart = "always";
        RestartSec = 3;
      };
    };
  };
}
