{ config, pkgs, options, ... }:
let
  my-python-packages = python-packages: with python-packages; [
    appdirs
    ipdb
    ipython
    numpy
    openpyxl
    pip
    python-language-server
    requests
    tensorflow
    tox
    virtualenv
    virtualenvwrapper
  ];
  python-with-my-packages = pkgs.python3.withPackages my-python-packages;
  all-hies = import (fetchTarball "https://github.com/infinisil/all-hies/tarball/master") {};
  notifications-tray-icon-source = pkgs.fetchFromGitHub {
    owner = "IvanMalison";
    repo = "notifications-tray-icon";
    rev = "f28288849a39feec8972a4181ce18ccdde6cc483";
    sha256 = "11r95m316x93bs1dj0bvas8adpd0xgql2jz8a8dnzv0fv4mw7aj4";
  };
  ntiOverlay = (import (notifications-tray-icon-source.outPath + "/overlay.nix"));
  ntiHaskellPackages = (ntiOverlay pkgs pkgs).haskellPackages;
  # XXX: Can't use pkgs here because it would cause infinite recursion
  lorriSource = (import <nixpkgs> {}).fetchurl {
    url = "https://raw.githubusercontent.com/target/lorri/master/direnv/nixos.nix";
    sha256 = "057kqbivf4xbhakz1j1b19sxd5c6p6rqhg6pwnq2zfvvmp8nmylm";
  };
  lorriBinSource = pkgs.fetchFromGitHub {
    owner = "target";
    repo = "lorri";
    rev = "80ca3e7c12f74af035cdeff289ba2aa3c8950cb2";
    sha256 = "05a0nrg9hp4li5nmyf4a5975p4amq19f17rqxncf7pcagyw0sax2";
  };
  lorri = (import (lorriBinSource.outPath + "/default.nix")) { inherit pkgs; };
in
{
  imports = [ lorriSource.outPath ];
  nixpkgs.overlays = [
    (import ./overlays.nix)
    (import ../dotfiles/config/xmonad/overlay.nix)
  ];

  # Allow all the things
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.android_sdk.accept_license = true;

  # Security and networking
  security.sudo.wheelNeedsPassword = false;
  networking.networkmanager = {
    enable = true;
    enableStrongSwan = true;
    packages = [ pkgs.networkmanager-l2tp ];
    extraConfig = ''
      [main]
      rc-manager=resolvconf
    '';
  };
  networking.firewall.enable = false;

  # Audio
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  hardware.opengl.driSupport32Bit = true;

  i18n = {
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  time.timeZone = "America/Los_Angeles";

  fonts = {
    fonts = with pkgs; [
      dejavu_fonts
      emojione
      fira-mono
      font-awesome-ttf
      noto-fonts-emoji
      roboto
      source-code-pro
      source-sans-pro
      source-serif-pro
      twemoji-color-font
    ];
    fontconfig = {
      defaultFonts = {
        monospace = [ "Source Code Pro" ];
        sansSerif = [ "Roboto" ];
        serif     = [ "Source Serif Pro" ];
      };
      ultimate = {
        enable = false;
      };
    };
  };

  xdg.menus.enable = true;

  environment.systemPackages = with pkgs; [

    # Applications
    calibre
    gnome3.cheese
    dfeet
    discord
    emacs
    firefox
    gitter
    google-chrome
    hexchat
    keybase-gui
    kleopatra
    kodi
    libreoffice
    lxappearance
    pulseeffects
    quassel
    rxvt_unicode
    slack
    simplescreenrecorder
    spotify
    termite
    vlc
    xfce.thunar
    wire-desktop
    zoom-us

    # Appearance
    gnome-breeze
    gnome3.adwaita-icon-theme
    hicolor-icon-theme
    materia-theme
    numix-icon-theme-circle
    plasma5.breeze-gtk
    plasma5.breeze-qt5

    # Haskell Desktop
    (import ../dotfiles/config/taffybar/default.nix)
    (import ../dotfiles/config/xmonad/default.nix)
    (ntiHaskellPackages.callCabal2nix "notifications-tray-icon" notifications-tray-icon-source { })
    haskellPackages.gtk-sni-tray
    haskellPackages.status-notifier-item
    haskellPackages.xmonad
    haskellPackages.dbus-hslogger

    # Desktop
    autorandr
    betterlockscreen
    blueman
    clipit
    compton
    feh
    gnome3.gpaste
    kdeconnect
    libnotify
    lxqt.lxqt-powermanagement
    networkmanagerapplet
    customizable-notify-osd

    pasystray-appindicator
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

    # Haskell
    cabal-install
    cabal2nix
    ghc
    stack
    haskellPackages.hpack
    haskellPackages.hasktags
    haskellPackages.hoogle
    (all-hies.selection { selector = p: { inherit (p) ghc864 ghc865; }; })

    # Scala
    sbt
    scala

    # Node
    nodePackages.npm
    nodejs

    # Rust
    cargo
    carnix
    racer
    rls
    rustc

    # Clojure
    boot
    leiningen

    # Ruby
    ruby

    # Tools
    automake
    bazaar
    bind
    binutils
    dex
    direnv
    dpkg
    emacs26Packages.cask
    fd
    file
    gcc
    gdb
    gitAndTools.git-crypt
    gitAndTools.git-fame
    gitAndTools.git-sync
    gitAndTools.hub
    gitFull
    glxinfo
    gnumake
    gnupg
    gparted
    htop
    inotify-tools
    ispell
    jq
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
    prometheus_2
    pscircle
    python-with-my-packages
    qt5.qttools
    rcm
    rr
    scrot
    silver-searcher
    stow
    tmux
    unzip
    usbutils
    valgrind
    wget

    # Nix
    nix-prefetch-git
    cachix
    lorri

    # Miscellaneous
    android-udev-rules
    librsvg
    transmission-gtk
  ];

  # XXX: Plasma seems to set this
  # environment.variables = {
  #   GDK_PIXBUF_MODULE_FILE = "${pkgs.librsvg.out}/lib/gdk-pixbuf-2.0/2.10.0/loaders.cache";
  # };

  # Enabling zsh will clobber path because of the way it sets up /etc/zshenv
  programs.zsh.enable = true;

  programs.gnupg.agent = { enable = true; enableSSHSupport = true; };
  programs.adb.enable = true;

  services.openssh.enable = true;

  services.avahi = {
    enable = true;
    nssmdns = true;
    publish = {
      enable = true;
      domain = true;
      userServices = true;
    };
  };

  services.gnome3.at-spi2-core.enable = true;

  services.kbfs.enable = true;

  services.autorandr.enable = true;

  services.locate.enable = true;

  services.xserver = {
    exportConfiguration = true;
    enable = true;
    layout = "us";
    desktopManager = {
      plasma5.enable = true;
      gnome3.enable = true;
      default = "none";
    };
    windowManager = {
      default = "xmonad";
      session = [{
        name = "xmonad";
        start = ''
          /usr/bin/env imalison-xmonad &
          waitPID=$!
        '';
      }];
    };
    displayManager = {
      sddm = {
        enable = true;
      };
      sessionCommands = ''
        systemctl --user import-environment GDK_PIXBUF_MODULE_FILE
      '';
    };
  };

  virtualisation.docker.enable = true;

  users.extraUsers = let
    extraGroups = [
      "audio"
      "adbusers"
      "disk"
      "docker"
      "networkmanager"
      "systemd-journal"
      "video"
      "wheel"
    ];
    userDefaults = {
      inherit extraGroups;
      group = "users";
      isNormalUser = true;
      createHome = true;
      shell = pkgs.zsh;
    };
  in {
    imalison = userDefaults // {
      name = "imalison";
      uid = 1000;
      home = "/home/imalison";
      shell = pkgs.zsh;
    };
    kat = userDefaults // {
      name = "kat";
      uid = 1001;
      home = "/home/kat";
      shell = pkgs.zsh;
    };
  };

  system.stateVersion = "18.03";
}
