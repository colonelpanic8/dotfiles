{ config, pkgs, options, inputs, ... }:
{
  imports = [
    ./fonts.nix
    inputs.home-manager.nixosModule
  ];
  nixpkgs.overlays = with inputs; [
    xmonad.overlay
    xmonad-contrib.overlay
    notifications-tray-icon.overlay
    (import ../dotfiles/config/xmonad/overlay.nix)
  ] ++ taffybar.overlays;

  services.autorandr.enable = true;

  services.xserver = {
    exportConfiguration = true;
    enable = true;
    layout = "us";
    desktopManager = {
      plasma5.enable = true;
    };
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
    displayManager = {
      sddm = {
        enable = true;
      };
      sessionCommands = ''
        systemctl --user import-environment GDK_PIXBUF_MODULE_FILE DBUS_SESSION_BUS_ADDRESS PATH
      '';
    };
  };

  environment.sessionVariables.NIXOS_OZONE_WL = "1";

  environment.systemPackages = with pkgs; [
    # Appearance
    gnome.adwaita-icon-theme
    hicolor-icon-theme
    libsForQt5.breeze-gtk
    materia-theme
    numix-icon-theme-circle
    papirus-icon-theme
    tela-icon-theme
    tela-circle-icon-theme

    # XOrg
    autorandr
    wmctrl
    xclip
    xdotool
    xorg.xev
    xorg.xkbcomp
    xorg.xwininfo
    xsettingsd

    # Haskell Desktop
    haskellPackages.xmonad
    haskellPackages.imalison-xmonad
    haskellPackages.notifications-tray-icon
    haskellPackages.gtk-sni-tray
    haskellPackages.status-notifier-item
    haskellPackages.dbus-hslogger

    # Desktop
    alacritty
    betterlockscreen
    blueman
    clipit
    dfeet
    discord
    dolphin
    element-desktop
    emacs
    feh
    firefox
    gitter
    gnome.cheese
    gnome.gpaste
    google-chrome
    hexchat
    keybase-gui
    kitty
    kleopatra
    libnotify
    libreoffice
    lxappearance
    lxqt.lxqt-powermanagement
    networkmanagerapplet
    notify-osd-customizable
    okular
    pasystray
    picom
    pinentry
    quassel
    rofi
    rofi-pass
    rofi-systemd
    simplescreenrecorder
    skippy-xd
    slack
    spotify
    synergy
    transmission-gtk
    vscode
    vlc
    volnoti
    wire-desktop
    xfce.thunar
    # zoom-us

    # Audio
    playerctl
    pulsemixer
    espeak
  ];

  home-manager.useGlobalPkgs = true;
  home-manager.useUserPackages = true;
  home-manager.users.imalison = (import ./home-manager.nix) inputs;
}
