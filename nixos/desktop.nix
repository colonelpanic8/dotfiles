{ config, pkgs, options, inputs, makeEnable, forEachUser, ... }:
makeEnable config "modules.desktop" true {
  imports = [
    ./fonts.nix
  ];

  services.xserver = {
    exportConfiguration = true;
    enable = true;
    layout = "us";
    desktopManager = {
      plasma5.enable = true;
    };
    displayManager = {
      sddm = {
        enable = true;
      };
      sessionCommands = ''
        systemctl --user import-environment GDK_PIXBUF_MODULE_FILE DBUS_SESSION_BUS_ADDRESS PATH
      '';
      setupCommands = ''
        autorandr -c
        systemctl restart autorandr.service
      '';
    };
  };

  services.autorandr = {
    enable = true;
  };

  # This is for the benefit of VSCODE running natively in wayland
  environment.sessionVariables.NIXOS_OZONE_WL = "1";

  services.gnome.at-spi2-core.enable = true;

  services.gnome.gnome-keyring.enable = true;

  environment.systemPackages = with pkgs; [
    # Appearance
    gnome.adwaita-icon-theme
    hicolor-icon-theme
    libsForQt5.breeze-gtk
    materia-theme
    numix-icon-theme-circle
    papirus-icon-theme

    # XOrg
    autorandr
    wmctrl
    xclip
    xdotool
    xorg.xev
    xorg.xkbcomp
    xorg.xwininfo
    xsettingsd

    # Desktop
    alacritty
    blueman
    clipit
    dfeet
    dolphin
    element-desktop

    feh
    firefox
    gnome.cheese
    gnome.gpaste

    kleopatra
    libnotify
    # Seems to be broken
    libreoffice
    lxappearance
    lxqt.lxqt-powermanagement
    networkmanagerapplet
    notify-osd-customizable
    okular
    picom
    pinentry
    psensor
    quassel
    remmina
    rofi
    rofi-pass
    rofi-systemd
    shutter
    simplescreenrecorder
    skippy-xd
    synergy
    transmission-gtk
    vlc
    volnoti
    xfce.thunar

    # Audio
    picard
    playerctl
    pulsemixer
    espeak

    # Visualization
    graphviz
    nodePackages.mermaid-cli
  ] ++ (if pkgs.system == "x86_64-linux" then with pkgs; [
    bitwarden
    discord
    etcher
    google-chrome
    keybase-gui
    pommed_light
    slack
    spicetify-cli
    spotify
    tor-browser-bundle-bin
    vscode
    zoom-us
  ] else []);

  home-manager.users = forEachUser (if pkgs.system == "x86_64-linux" then {
    systemd.user.services.bitwarden = {
      Unit = {
        Description = "Bitwarden";
        After = [ "graphical-session-pre.target" "tray.target" ];
        PartOf = [ "graphical-session.target" ];
      };

      Install = { WantedBy = [ "graphical-session.target" ]; };

      Service = {
        ExecStart = "${pkgs.bitwarden}/bin/bitwarden";
        Restart = "always";
        RestartSec = 3;
      };
    };
  } else {});
}
