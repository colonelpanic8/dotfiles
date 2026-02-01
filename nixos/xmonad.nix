{ config, pkgs, inputs, forEachUser, makeEnable, ... }:
makeEnable config "myModules.xmonad" true  {
  nixpkgs.overlays = with inputs; [
    xmonad.overlay
    xmonad-contrib.overlay
    notifications-tray-icon.overlay
    (import ../dotfiles/config/xmonad/overlay.nix)
  ] ++ taffybar.overlays;

  services.rumno.enable = true;

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
    # haskellPackages.notifications-tray-icon
    # haskellPackages.gtk-sni-tray
    haskellPackages.status-notifier-item
    haskellPackages.dbus-hslogger
    inputs.imalison-taffybar.defaultPackage."${pkgs.stdenv.hostPlatform.system}"
  ];

  home-manager.users = forEachUser {
    imports = [ ./dunst.nix ];

    services.blueman-applet = {
      enable = true;
    };

    services.taffybar = {
      enable = true;
      package = inputs.imalison-taffybar.defaultPackage."${pkgs.stdenv.hostPlatform.system}";
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

    services.autorandr.enable = true;

    services.random-background = {
      enable = true;
      display = "fill";
      interval = "1h";
      imageDirectory = "/var/lib/syncthing/sync/Wallpaper/";
    };

    services.xsettingsd.enable = true;

    services.pasystray.enable = true;

    # services.parcellite = {
    #   enable = true;
    #   package = pkgs.clipit;
    # };

    services.flameshot = {
      enable = true;
    };

    services.picom = {
      enable = true;
      vSync = config.myModules.xmonad.picom.vSync.enable;
      backend = "glx";

      settings = {
        # Note: animations must use () not [] in libconfig, so we put them in extraArgs
        # via xdg.configFile below

        fade-in-step = 0.028;
        fade-out-step = 0.028;
        fading = true;

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

    # Spring physics animations config - written separately because home-manager
    # generates [] (arrays) but picom needs () (lists) for animations
    xdg.configFile."picom/animations.conf".text = ''
      # Spring physics animations (mainline picom with spring-physics branch)
      # Syntax: spring(stiffness, dampening, mass) or spring(stiffness, dampening, mass, clamping)
      # Set clamping to false for bounce/overshoot effects
      animations = (
        # Window move/resize animation with spring physics
        {
          triggers = ["geometry"];
          offset-x = {
            curve = "spring(250, 20, 1, false)";
            start = "window-x-before - window-x";
            end = 0;
          };
          offset-y = {
            curve = "spring(250, 20, 1, false)";
            start = "window-y-before - window-y";
            end = 0;
          };
        },
        # Opacity fade animation
        {
          triggers = ["open", "show"];
          opacity = {
            curve = "cubic-bezier(0.25, 0.1, 0.25, 1)";
            duration = 0.2;
            start = 0;
            end = "window-raw-opacity";
          };
          scale-x = {
            curve = "cubic-bezier(0.25, 0.1, 0.25, 1)";
            duration = 0.2;
            start = 0.9;
            end = 1;
          };
          scale-y = {
            curve = "cubic-bezier(0.25, 0.1, 0.25, 1)";
            duration = 0.2;
            start = 0.9;
            end = 1;
          };
        },
        {
          triggers = ["close", "hide"];
          opacity = {
            curve = "cubic-bezier(0.25, 0.1, 0.25, 1)";
            duration = 0.2;
            start = "window-raw-opacity";
            end = 0;
          };
          scale-x = {
            curve = "cubic-bezier(0.25, 0.1, 0.25, 1)";
            duration = 0.2;
            start = 1;
            end = 0.9;
          };
          scale-y = {
            curve = "cubic-bezier(0.25, 0.1, 0.25, 1)";
            duration = 0.2;
            start = 1;
            end = 0.9;
          };
        }
      );
    '';

    # Override picom service to include animations config
    # Use %h for home directory since XDG_CONFIG_HOME may not be set
    systemd.user.services.picom = {
      Service.ExecStart = pkgs.lib.mkForce "${pkgs.picom}/bin/picom --config %h/.config/picom/picom.conf --config %h/.config/picom/animations.conf";
    };

    # systemd.user.services.notifications-tray-icon = {
    #   Unit = {
    #     Description = "Notifications tray icon";
    #     After = [ "graphical-session-pre.target" "tray.target" ];
    #     PartOf = [ "graphical-session.target" ];
    #   };

    #   Install = { WantedBy = [ "graphical-session.target" ]; };

    #   Service = {
    #     ExecStart = "${pkgs.haskellPackages.notifications-tray-icon}/bin/notifications-tray-icon  --github-token-pass dfinity-github-api-token";
    #     Restart = "always";
    #     RestartSec = 3;
    #   };
    # };
  };
}
