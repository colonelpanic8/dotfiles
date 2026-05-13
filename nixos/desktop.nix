{
  inputs,
  config,
  pkgs,
  lib,
  makeEnable,
  ...
}: let
  cfg = config.myModules.desktop;
  desktopShellUi = pkgs.writeShellApplication {
    name = "desktop_shell_ui";
    runtimeInputs = [
      pkgs.bash
      pkgs.coreutils
      pkgs.systemd
    ];
    text = ''
      exec ${../dotfiles/lib/bin/desktop_shell_ui} "$@"
    '';
  };
  googleChrome = pkgs.symlinkJoin {
    name = "google-chrome-wayland-fractional-scale-workaround";
    paths = [pkgs.google-chrome];
    nativeBuildInputs = [pkgs.makeWrapper];
    postBuild = ''
      wrapProgram "$out/bin/google-chrome-stable" \
        --add-flags "--disable-features=WaylandFractionalScaleV1"

      desktop_file="$out/share/applications/google-chrome.desktop"
      rm "$desktop_file"
      cp "${pkgs.google-chrome}/share/applications/google-chrome.desktop" "$desktop_file"
      chmod u+w "$desktop_file"

      substituteInPlace "$desktop_file" \
        --replace-fail \
          "Exec=${pkgs.google-chrome}/bin/google-chrome-stable" \
          "Exec=$out/bin/google-chrome-stable"
    '';
  };
  enabledModule = makeEnable config "myModules.desktop" true {
    services.greenclip.enable = true;
    imports = [
      ./fonts.nix
      ./hyprland.nix
      ./keyd.nix
      ./wlsunset.nix
    ];

    assertions = [
      {
        assertion = !(config.myModules.taffybar.enable && config.myModules.waybar.enable);
        message = "myModules.taffybar and myModules.waybar cannot both be enabled.";
      }
    ];

    services.xserver = {
      exportConfiguration = true;
      enable = true;
      displayManager = {
        sessionCommands = ''
          systemctl --user import-environment GDK_PIXBUF_MODULE_FILE DBUS_SESSION_BUS_ADDRESS PATH
        '';
      };
    };

    environment.sessionVariables = {
      # This is for the benefit of VSCODE running natively in wayland
      NIXOS_OZONE_WL = "1";
      IM_HYPRLAND_SHELL_UI = cfg.shellUi;
    };

    system.activationScripts.playwrightChromeCompat.text = lib.optionalString (pkgs.stdenv.hostPlatform.system == "x86_64-linux") ''
      # Playwright's Chrome channel lookup expects the FHS path below.
      mkdir -p /opt/google/chrome
      ln -sfn ${googleChrome}/bin/google-chrome-stable /opt/google/chrome/chrome
    '';

    services.gnome.at-spi2-core.enable = true;

    services.gnome.gnome-keyring.enable = true;

    home-manager.sharedModules = [
      {
        imports = [./dunst.nix];

        xdg.desktopEntries."com.mitchellh.ghostty" = {
          name = "Ghostty";
          comment = "A terminal emulator";
          icon = "com.mitchellh.ghostty";
          terminal = false;
          type = "Application";
          categories = ["System" "TerminalEmulator"];
          startupNotify = true;
          exec = "${pkgs.ghostty}/bin/ghostty --gtk-single-instance=false";
          settings = {
            StartupWMClass = "com.mitchellh.ghostty";
            X-GNOME-UsesNotifications = "true";
            X-TerminalArgExec = "-e";
            X-TerminalArgTitle = "--title=";
            X-TerminalArgAppId = "--class=";
            X-TerminalArgDir = "--working-directory=";
            X-TerminalArgHold = "--wait-after-command";
          };
          actions = {
            new-window = {
              name = "New Window";
              exec = "${pkgs.ghostty}/bin/ghostty --gtk-single-instance=false";
            };
          };
        };

        xdg.configFile."ghostty/config" = {
          force = true;
          text = ''
            confirm-close-surface = false
            shell-integration-features = cursor,no-sudo,no-title,no-ssh-env,no-ssh-terminfo,path
          '';
        };

        xdg.configFile."ghostty/dropdown" = {
          force = true;
          text = ''
            config-file = /home/imalison/.config/ghostty/config
            class = com.mitchellh.ghostty.dropdown
            title = dropdown
            gtk-single-instance = false
            window-decoration = none
            window-padding-x = 0
            window-padding-y = 0
            background-opacity = 0.82
            background-opacity-cells = true
            background-blur = true
          '';
        };
      }
    ];

    environment.systemPackages = with pkgs;
      [
        desktopShellUi

        # Appearance
        adwaita-icon-theme
        hicolor-icon-theme
        # libsForQt5.breeze-gtk
        # materia-theme
        kdePackages.qt6ct
        libsForQt5.qt5ct
        numix-icon-theme-circle
        papirus-icon-theme

        # XOrg
        autorandr
        keyd
        wmctrl
        xclip
        xdotool
        xev
        xwininfo
        xsettingsd

        # Desktop
        alacritty
        ghostty
        blueman
        # clipit
        d-spy
        kdePackages.dolphin

        feh
        file-roller
        gthumb
        firefox
        cheese
        kdePackages.kleopatra
        libnotify
        libreoffice
        loupe
        lxappearance
        lxqt.lxqt-powermanagement
        networkmanagerapplet
        kdePackages.okular
        pinentry-gnome3
        # mission-center
        quassel
        remmina
        rofi
        wofi
        rofi-pass
        rofi-systemd
        simplescreenrecorder
        skippy-xd
        transmission_4-gtk
        vlc
        thunar

        # Audio
        picard
        pavucontrol
        playerctl
        pulsemixer
        espeak

        #
        brightnessctl

        # Visualization
        graphviz
        mermaid-cli
        pandoc
      ]
      ++ (
        if pkgs.stdenv.hostPlatform.system == "x86_64-linux"
        then
          with pkgs; [
            googleChrome
            pommed_light
            slack
            spicetify-cli
            spotify
            tor-browser
            vscode
            zulip
          ]
        else []
      );
  };
in
  enabledModule
  // {
    options = lib.recursiveUpdate enabledModule.options {
      myModules.desktop.shellUi = lib.mkOption {
        type = lib.types.enum ["taffybar"];
        default = "taffybar";
        description = ''
          Desktop shell UI used by Hyprland-oriented bindings. This controls
          the active shell service and Hyprland launcher/window picker dispatch.
        '';
      };
    };
  }
