{ pkgs, ... }:
let
  my-python-packages = python-packages: with python-packages; [
    appdirs
    ipdb
    ipython
    numpy
    openpyxl
    pip
    requests
    tox
    virtualenv
    virtualenvwrapper
  ];
  python-with-my-packages = pkgs.python3.withPackages my-python-packages;
in
{
  environment.systemPackages = with pkgs; [
    python-with-my-packages

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
    # direnv
    gitFull
    rcm
    ripgrep
    silver-searcher
    tmux
    usbutils
    wget
    yubikey-manager
  ];
}
