{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    emacs
    firefox
    vlc
    transmission-gtk
    dolphin

    # Appearance
    hicolor-icon-theme

    # XOrg
    wmctrl
    xclip
    xdotool
    xorg.xev
    xorg.xkbcomp
    xorg.xwininfo
    xsettingsd

    # Haskell Desktop
    haskellPackages.imalison-xmonad
    haskellPackages.imalison-taffybar
    haskellPackages.status-notifier-item
    haskellPackages.dbus-hslogger

    # Desktop
    autorandr
    betterlockscreen
    blueman
    clipit
    feh
    gnome3.gpaste
    kdeconnect
    libnotify
    lxqt.lxqt-powermanagement
    networkmanagerapplet
    notify-osd-customizable
    pasystray
    picom
    pinentry
    rofi
    rofi-pass
    rofi-systemd
    skippy-xd
    synergy
    udiskie
    volnoti

    # Audio
    playerctl

    # Tools
    binutils
    gitFull
    rcm
    silver-searcher
    ripgrep
    usbutils
    tmux
    wget
    yubikey-manager
  ];
}
