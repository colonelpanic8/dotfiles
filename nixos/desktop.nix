{ config, pkgs, options, inputs, ... }:
{
  imports = [
    ./fonts.nix
    inputs.home-manager.nixosModule
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
    };
  };

  services.autorandr = {
    enable = true;
  };

  systemd.services.autorandr-startup = {
    partOf = [ "graphical-session.target" ];
    description = "autorandr";

    serviceConfig = {
      Type = "oneshot";
      ExecStart = "${pkgs.autorandr}/bin/autorandr --change";
    };

    wantedBy = [ "graphical-session.target" ];
  };

  # This is for the benefit of VSCODE running natively in wayland
  environment.sessionVariables.NIXOS_OZONE_WL = "1";

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
    playerctl
    pulsemixer
    espeak
  ];
}
