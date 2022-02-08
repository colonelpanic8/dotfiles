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
  nixpkgs.overlays = [
    (import ../dotfiles/config/xmonad/overlay.nix)
  ];
  environment.systemPackages = with pkgs; [
    python-with-my-packages

    alacritty
    (emacsWithPackages (epkgs: with epkgs.melpaPackages; [
      vterm
    ]))
    firefox
    vlc
    transmission-gtk
    dolphin
    element-desktop
    gpick

    # Appearance
    numix-icon-theme-circle
    papirus-icon-theme
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
    haskellPackages.xmonad
    haskellPackages.imalison-xmonad
    haskellPackages.imalison-taffybar
    haskellPackages.notifications-tray-icon
    haskellPackages.gtk-sni-tray
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
    gawk
    gitFull
    git-sync
    htop
    ncdu
    lsof
    pass
    protobuf
    rcm
    ripgrep
    silver-searcher
    tzupdate
    tmux
    usbutils
    wget
    yubikey-manager
  ];
}
