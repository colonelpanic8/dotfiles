{ config, pkgs, options, ... }:
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
  all-hies = import (fetchTarball "https://github.com/infinisil/all-hies/tarball/master") {};
  notifications-tray-icon-source = pkgs.fetchFromGitHub {
    owner = "IvanMalison";
    repo = "notifications-tray-icon";
    rev = "a855ebf924af3d695c5a10caca34b4eb88f58afb";
    sha256 = "1pd7jhapz080v9q9iv7g8jk9an24zkipmfgg9fmfjr1qjv1zdbib";
  };
  notifications-tray-icon = (import (notifications-tray-icon-source.outPath + "/default.nix"));
in
{
  nixpkgs.overlays = [
    (import ./overlays.nix)
    (import ../dotfiles/config/taffybar/taffybar/environment.nix)
  ];

  # Allow all the things
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.android_sdk.accept_license = true;

  nixpkgs.config.permittedInsecurePackages = [
    "openssl-1.0.2u"
  ];


  # Security and networking
  security.sudo.wheelNeedsPassword = false;

  environment.etc."ipsec.secrets".text = ''
    include ipsec.d/ipsec.nm-l2tp.secrets
  '';
  networking.networkmanager = {
    enable = true;
    enableStrongSwan = true;
    packages = [ pkgs.networkmanager-l2tp ];
    extraConfig = ''
      [main]
      rc-manager=resolvconf
    '';
  };
  # Disabling these waits disables the stuck on boot up issue
  systemd.services.systemd-udev-settle.enable = false;
  systemd.services.NetworkManager-wait-online.enable = false;
  networking.firewall.enable = false;

  # Audio
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  hardware.opengl.driSupport32Bit = true;

  # bluetooth
  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  console.keyMap = "us";

  i18n = {
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
    };
  };

  xdg.menus.enable = true;

  environment.systemPackages = with pkgs; [

    # Applications
    alacritty
    # calibre
    gnome3.cheese
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
    rxvt_unicode
    slack
    simplescreenrecorder
    spotify
    vlc
    xfce.thunar
    wire-desktop
    yubikey-manager
    zoom-us

    # Appearance
    gnome-breeze
    gnome3.adwaita-icon-theme
    hicolor-icon-theme
    materia-theme
    numix-icon-theme-circle
    papirus-icon-theme
    plasma5.breeze-gtk
    plasma5.breeze-qt5

    # Haskell Desktop
    (import ../dotfiles/config/taffybar/default.nix)
    (import ../dotfiles/config/xmonad/default.nix)
    # notifications-tray-icon
    haskellPackages.status-notifier-item
    haskellPackages.xmonad
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
    # stack
    haskellPackages.hpack
    haskellPackages.hasktags
    haskellPackages.hoogle
    # (all-hies.selection { selector = p: { inherit (p) ghc864 ghc865; }; })

    # Scala
    sbt
    scala

    # Node
    nodePackages.npm
    nodejs

    # Rust
    cargo
    carnix
    # rls
    rustc

    # Clojure
    boot
    leiningen

    # Ruby
    ruby

    # purescript
    purescript
    spago

    # dhall
    haskellPackages.dhall
    haskellPackages.dhall-json

    # Tools
    automake
    bind
    binutils
    cmake
    dex
    direnv
    dpkg
    emacs26Packages.cask
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
    lorri
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
    python-with-my-packages
    qt5.qttools
    rcm
    rr
    scrot
    silver-searcher
    stow
    subversion
    tmux
    unzip
    usbutils
    valgrind
    wget

    # Nix
    nix-prefetch-git
    cachix

    # Miscellaneous
    android-udev-rules
    librsvg
    transmission-gtk
  ];

  # XXX: Plasma seems to set this
  # environment.variables = {
  #   GDK_PIXBUF_MODULE_FILE = "${pkgs.librsvg.out}/lib/gdk-pixbuf-2.0/2.10.0/loaders.cache";
  # };

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

  services.lorri.enable = true;

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
        {
          name = "waymonad";
          start = ''
            /usr/bin/env waymonad
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
  nix.trustedUsers = ["imalison"];
}
