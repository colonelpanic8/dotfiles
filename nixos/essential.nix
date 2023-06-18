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

  nixpkgs.config.allowBroken = true;
  programs.hyprland.enable = true;
  environment.systemPackages = with pkgs; [
    python-with-my-packages
    alacritty
    (emacs29.override {
      withNativeCompilation = true;
      withTreeSitter = true;
    })
    vscode
    firefox
    kitty
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
    # haskellPackages.notifications-tray-icon
    haskellPackages.gtk-sni-tray
    haskellPackages.status-notifier-item
    haskellPackages.dbus-hslogger

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
    automake
    bazel
    bind
    binutils
    binutils
    cmake
    dex
    direnv
    direnv
    dpkg
    fd
    file
    gawk
    gcc
    gdb
    git-sync
    gitFull
    htop
    jq
    lsof
    ncdu
    nix-index
    pass
    pciutils
    protobuf
    rclone
    rcm
    ripgrep
    silver-searcher
    tmux
    tzupdate
    unzip
    usbutils
    wget
    yubikey-manager
  ];
}
