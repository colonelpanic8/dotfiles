{ config, pkgs, ... }:
let
  gitter = with pkgs; callPackage ./gitter.nix { };
  my-python-packages = python-packages: with python-packages; [
    appdirs
    requests
    virtualenv
    ipython
    ipdb
    virtualenvwrapper
    pip
  ];
  python-with-my-packages = pkgs.python3.withPackages my-python-packages;
  udiskie-appindicator = pkgs.udiskie.overrideAttrs (oldAttrs: rec {
    version = "1.7.5";
    src = pkgs.fetchFromGitHub {
      owner = "coldfix";
      repo = "udiskie";
      rev = version;
      sha256 = "1mcdn8ha5d5nsmrzk6xnnsqrmk94rdrzym9sqm38zk5r8gpyl1k4";
    };
    propagatedBuildInputs = oldAttrs.propagatedBuildInputs ++ [pkgs.libappindicator-gtk3];
  });
  clipit-master = pkgs.clipit.overrideAttrs (oldAttrs: rec {
    version = "50d983514386029a1f133187902084b753458f32";
    preConfigure = "./autogen.sh";
    configureFlags = ["--with-gtk3" "--enable-appindicator"];
    src = pkgs.fetchFromGitHub {
      owner = "IvanMalison";
      repo = "ClipIt";
      sha256 = "1d52zjnxmcp2kr4wvq2yn9fhr61v9scp91fxfvasvz5m7k1zagdn";
      rev = version;
    };
    buildInputs = with pkgs; [
      autoconf automake intltool gtk3 xdotool hicolor-icon-theme
      libappindicator-gtk3
    ];
  });
  pasystray-appindicator = with pkgs; pasystray.overrideAttrs (oldAttrs: rec {
    buildInputs = oldAttrs.buildInputs ++ [libappindicator-gtk3];
  });
  customizable-notify-osd = with pkgs; notify-osd.overrideAttrs (oldAttrs: rec {
    version = "0.9.35+16.04.20160415";
    baseURI = "https://launchpad.net/~leolik/+archive/leolik";
    src = fetchurl {
      url = "${baseURI}/+files/notify-osd_${version}-0ubuntu1-leolik~ppa0.tar.gz";
      sha256 = "026dr46jh3xc4103wnslzy7pxbxkkpflh52c59j8vzwaa7bvvzkv";
      name = "notify-osd-customizable.tar.gz";
    };
    preConfigure = "./autogen.sh --libexecdir=$(out)/bin";
    buildInputs = with pkgs; [
      glib libwnck3 libnotify dbus-glib gnome3.gsettings-desktop-schemas
      makeWrapper libtool gnome3.gnome-common
    ];
  });
  keybase-gui-fixed = with pkgs; keybase-gui.overrideAttrs (oldAttrs: rec {
    installPhase = ''
    mkdir -p $out/bin
    mv usr/share $out/share
    mv opt/keybase $out/share/
    cat > $out/bin/keybase-gui <<EOF
    #!${stdenv.shell}
    checkFailed() {
      if [ "\$NIX_SKIP_KEYBASE_CHECKS" = "1" ]; then
        return
      fi
      echo "Set NIX_SKIP_KEYBASE_CHECKS=1 if you want to skip this check." >&2
      exit 1
    }
    if [ ! -S "\$XDG_RUNTIME_DIR/keybase/keybased.sock" ]; then
      echo "Keybase service doesn't seem to be running." >&2
      echo "You might need to run: keybase service" >&2
      checkFailed
    fi
    if [ -z "\$(keybase status | grep kbfsfuse)" ]; then
      echo "Could not find kbfsfuse client in keybase status." >&2
      echo "You might need to run: kbfsfuse" >&2
      checkFailed
    fi
    exec $out/share/keybase/Keybase "\$@"
    EOF
    chmod +x $out/bin/keybase-gui
    substituteInPlace $out/share/applications/keybase.desktop \
      --replace run_keybase $out/bin/keybase-gui
  '';
  });
in
{
  nixpkgs.config.allowUnfree = true;
  security.sudo.wheelNeedsPassword = false;
  networking.networkmanager.enable = true;
  networking.firewall.enable = false;

  i18n = {
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  # Set your time zone.
  # TODO: this should be set dynamically
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

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [

    # Applications
    calibre
    discord
    emacs
    firefox
    kleopatra
    gitter
    google-chrome

    hexchat
    quassel
    keybase-gui-fixed
    kodi
    lxappearance
    rxvt_unicode
    spotify
    termite
    vlc
    xfce.thunar

    # Appearance
    numix-icon-theme-circle
    gnome3.adwaita-icon-theme
    hicolor-icon-theme
    plasma5.breeze-gtk
    plasma5.breeze-qt5
    gnome-breeze

    # Desktop
    # haskellPackages.status-notifier-item
    autorandr
    clipit-master
    compton
    feh
    gnome3.gpaste
    kdeconnect
    libnotify
    # XXX: renable this
    # lxqt.lxqt-powermanagement
    networkmanagerapplet
    customizable-notify-osd
    pasystray-appindicator
    pinentry
    pommed_light
    rofi
    rofi-pass
    skippy-xd
    synergy
    udiskie-appindicator
    volnoti
    xclip
    xdotool
    xorg.xkbcomp
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

    # Scala
    sbt
    scala

    # Node
    nodePackages.npm
    nodejs

    # Rust
    cargo

    # Tools
    bazaar
    binutils
    dfeet
    dpkg
    gcc
    gdb
    gitAndTools.git-sync
    gitAndTools.git-fame
    gitAndTools.hub
    gitFull
    gnumake
    gnupg
    htop
    inotify-tools
    ispell
    mercurial
    ncdu
    neofetch
    pass
    patchelf
    plasma-workspace
    powertop
    python-with-my-packages
    qt5.qttools
    rcm
    scrot
    silver-searcher
    stow
    tmux
    valgrind
    wget
    wmctrl
    xorg.xev
    zsh

    # Nix
    nix-prefetch-git

    # Miscellaneous
    android-udev-rules
    librsvg
    transmission-gtk
  ];

  # XXX: Plasma seems to set this
  # environment.variables = {
  #   GDK_PIXBUF_MODULE_FILE = "${pkgs.librsvg.out}/lib/gdk-pixbuf-2.0/2.10.0/loaders.cache";
  # };

  programs.gnupg.agent = { enable = true; enableSSHSupport = true; };
  # Enabling zsh will clobber path because of the way it sets up /etc/zshenv
  # programs.zsh.enable = true;
  # Instead we just make sure to source profile from zsh
  environment.etc."zshenv".text =
    ''
      if [ -n "$__ETC_PROFILE_DONE" ]; then return; fi
      source /etc/profile
    '';
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
  services.kbfs.enable = true;
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  services.xserver = {
    exportConfiguration = true;
    enable = true;
    layout = "us";
    desktopManager = {
      plasma5.enable = true;
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
      # lightdm = {
      #   enable = true;
      #   extraSeatDefaults=''
      #     greeter-hide-users=false
      #   '';
      # };
      sddm = {
        enable = true;
      };
      sessionCommands = ''
        systemctl --user import-environment GDK_PIXBUF_MODULE_FILE
      '';
    };

  };

  hardware.opengl.driSupport32Bit = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers = let
    extraGroups = [
      "wheel" "disk" "audio" "video" "networkmanager" "systemd-journal"
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

  system.nixos.stateVersion = "18.03";
}
