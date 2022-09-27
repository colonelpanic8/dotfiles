{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    # Applications
    alacritty
    # calibre
    gnome.cheese
    dfeet
    discord
    emacs
    firefox
    gitter
    google-chrome
    hexchat
    keybase-gui
    kitty
    kleopatra
    # kodi
    libreoffice
    lxappearance
    okular
    # pulseeffects
    quassel
    slack
    simplescreenrecorder
    spotify
    transmission-gtk
    vlc
    xfce.thunar
    wire-desktop
    yubikey-manager
    zoom-us

    # Appearance
    libsForQt5.breeze-gtk
    gnome.adwaita-icon-theme
    hicolor-icon-theme
    materia-theme
    numix-icon-theme-circle
    papirus-icon-theme
    # plasma5.breeze-gtk
    # plasma5.breeze-qt5

    # Desktop
    autorandr
    betterlockscreen
    blueman
    clipit
    feh
    gnome.gpaste
    libnotify
    lxqt.lxqt-powermanagement
    networkmanagerapplet
    notify-osd-customizable
    pasystray
    picom
    pinentry
    pommed_light
    rofi
    rofi-pass
    rofi-systemd
    skippy-xd
    synergy
    udiskie
    volnoti

    # xorg
    wmctrl
    xclip
    xdotool
    xorg.xev
    xorg.xkbcomp
    xorg.xwininfo
    xsettingsd

    # Audio
    pulsemixer
    pavucontrol
    playerctl

    # Tools
    automake
    bind
    binutils
    cmake
    dex
    direnv
    dpkg
    fd
    file
    gcc
    gdb
    gitAndTools.git-crypt
    gitAndTools.git-extras
    gitAndTools.git-fame
    gitAndTools.git-sync
    gitAndTools.hub
    gitFull
    glxinfo
    gnumake
    gnupg
    gparted
    htop
    inetutils
    inotify-tools
    ispell
    jq
    libtool
    lsof
    mercurial
    ncdu
    neofetch
    openvpn
    parallel
    pass
    patchelf
    pciutils
    plasma-workspace
    pijul
    powertop
    prometheus
    pscircle
    pstree
    qt5.qttools
    rcm
    # rr
    scrot
    silver-searcher
    stow
    subversion
    swig
    tmux
    tzupdate
    unzip
    usbutils
    valgrind
    wget
    yubikey-manager
    rustup

    # Nix
    nix-prefetch-git
    # cachix

    # Miscellaneous
    android-udev-rules
    librsvg
  ];
}
