{ config, pkgs, inputs, makeEnable, ... }:
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

  home-manager.sharedModules = [
    {
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

      # Completely disable home-manager's picom - we manage everything ourselves
      # to work around the libconfig list vs array syntax issue for animations
      services.picom.enable = false;

      # Our own picom systemd service
      systemd.user.services.picom = {
        Unit = {
          Description = "Picom X11 compositor";
          After = [ "graphical-session.target" ];
          PartOf = [ "graphical-session.target" ];
        };
        Service = {
          # Debug logging to file for monitoring
          ExecStart = "${pkgs.picom}/bin/picom --config %h/.config/picom/picom.conf --log-level=debug --log-file=%h/.local/share/picom/debug.log";
          Restart = "always";
          RestartSec = 3;
        };
        Install = {
          WantedBy = [ "graphical-session.target" ];
        };
      };

      # Ensure log directory exists
      xdg.dataFile."picom/.keep".text = "";

      # Write complete picom config directly to avoid home-manager's libconfig generator
      # which incorrectly uses [] instead of () for the animations list
      xdg.configFile."picom/picom.conf" = {
        force = true;  # Override home-manager's generated config
        text = ''
          # Backend and basic settings
          backend = "glx";
          vsync = ${if config.myModules.xmonad.picom.vSync.enable then "true" else "false"};

          # Spring physics animations (mainline picom with spring-physics branch)
          # Syntax: spring(stiffness, dampening, mass) or spring(stiffness, dampening, mass, clamping)
          # Set clamping to false for bounce/overshoot effects
          animations = (
            # Window move/resize animation with spring physics
            # "geometry" is alias for "size" + "position" triggers
            # Suppress opacity changes so they don't interrupt geometry animations
            {
              triggers = ["geometry"];
              suppressions = ["decrease-opacity", "increase-opacity"];
              offset-x = {
                curve = "spring(150, 18, 1.5, false)";
                start = "window-x-before - window-x";
                end = 0;
              };
              offset-y = {
                curve = "spring(150, 18, 1.5, false)";
                start = "window-y-before - window-y";
                end = 0;
              };
              scale-x = {
                curve = "spring(150, 18, 1.5, false)";
                start = "window-width-before / window-width";
                end = 1;
              };
              scale-y = {
                curve = "spring(150, 18, 1.5, false)";
                start = "window-height-before / window-height";
                end = 1;
              };
            },
            # Window open/show animation with spring physics
            {
              triggers = ["open", "show"];
              # Opacity uses spring with clamping to prevent going above 1
              opacity = {
                curve = "spring(150, 18, 1.5, true)";
                start = 0;
                end = "window-raw-opacity";
              };
              # Scale uses spring with bounce for a nice "pop" effect
              scale-x = {
                curve = "spring(150, 18, 1.5, false)";
                start = 0.85;
                end = 1;
              };
              scale-y = {
                curve = "spring(150, 18, 1.5, false)";
                start = 0.85;
                end = 1;
              };
              # Center the scaling effect
              offset-x = "(1 - scale-x) / 2 * window-width";
              offset-y = "(1 - scale-y) / 2 * window-height";
            },
            # Window close/hide animation with spring physics
            {
              triggers = ["close", "hide"];
              opacity = {
                curve = "spring(150, 18, 1.5, true)";
                start = "window-raw-opacity";
                end = 0;
              };
              scale-x = {
                curve = "spring(150, 18, 1.5, true)";
                start = 1;
                end = 0.9;
              };
              scale-y = {
                curve = "spring(150, 18, 1.5, true)";
                start = 1;
                end = 0.9;
              };
              # Center the scaling effect
              offset-x = "(1 - scale-x) / 2 * window-width";
              offset-y = "(1 - scale-y) / 2 * window-height";
            }
          );

          # Fading
          fading = true;
          fade-in-step = 0.028;
          fade-out-step = 0.028;

          # Corners
          corner-radius = 10;
          round-borders = 0;
          rounded-corners-exclude = [
            "! name~='''",
            "window_type = 'dock'",
            "window_type = 'desktop'",
            "class_g ?= 'Dunst'"
        ];
        round-borders-exclude = [
          "! name~='''"
        ];

        # Focus and opacity
        focus-exclude = ["class_g ?= 'rofi'", "class_g ?= 'Steam'"];
        invert-color-include = [];

        # Detection
        detect-rounded-corners = true;
        detect-client-opacity = true;
        detect-transient = true;
        detect-client-leader = true;
        mark-wmwin-focused = false;
        mark-ovredir-focused = false;

        # Other settings
        daemon = false;
        dbus = false;
        unredir-if-possible = false;
        unredir-if-possible-exclude = [];
        use-damage = false;
        transparent-clipping = false;
      '';
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
    }
  ];
}
