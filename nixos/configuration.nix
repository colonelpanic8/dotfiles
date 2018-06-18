{ config, pkgs, ... }:
let
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
    version = "9741c39382a3f6e4c03eac6905a49794d07c465a";
    preConfigure = "./autogen.sh";
    configureFlags = ["--with-gtk3" "--enable-appindicator"];
    src = pkgs.fetchFromGitHub {
      owner = "IvanMalison";
      repo = "ClipIt";
      sha256 = "13lddvbsp16nir9ibllr403qxhwyh4h2bh6774icbb250pghykjx";
      rev = version;
    };
    buildInputs = with pkgs; [
      autoconf automake intltool gtk3 xdotool hicolor-icon-theme
      libappindicator-gtk3
    ];
  });
  git-sync = with pkgs; stdenv.mkDerivation rec {
    name = "git-sync-${version}";
    version = "20151024";

    src = fetchFromGitHub {
      owner = "simonthum";
      repo = "git-sync";
      rev = "eb9adaf2b5fd65aac1e83d6544b9076aae6af5b7";
      sha256 = "01if8y93wa0mwbkzkzx2v1vqh47zlz4k1dysl6yh5rmppd1psknz";
    };

    buildInputs = [ makeWrapper ];

    dontBuild = true;

    installPhase = ''
      mkdir -p $out/bin
      cp -a git-sync $out/bin/git-sync
    '';

    wrapperPath = with stdenv.lib; makeBinPath [
      coreutils
      git
      gnugrep
      gnused
    ];

    fixupPhase = ''
      patchShebangs $out/bin

      wrapProgram $out/bin/git-sync \
        --prefix PATH : "${wrapperPath}"
      '';
  };
in
{
  nixpkgs.config.allowUnfree = true;
  security.sudo.wheelNeedsPassword = false;
  networking.networkmanager.enable = true;

  i18n = {
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  # Set your time zone.
  # TODO: this should be set dynamically
  time.timeZone = "America/Los_Angeles";

  fonts = {
    fonts = with pkgs; [
      fira-mono
      dejavu_fonts
      noto-fonts-emoji
      emojione
      twemoji-color-font
      source-code-pro
      roboto
      source-sans-pro
      source-serif-pro
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
    emacs
    firefox
    google-chrome
    hexchat
    keybase-gui
    kodi
    rxvt_unicode
    spotify
    steam
    termite
    vlc
    xfce.thunar

    # Appearance
    numix-icon-theme-circle
    gnome3.adwaita-icon-theme
    hicolor-icon-theme

    # Desktop
    autorandr
    clipit-master
    compton
    feh
    gnome3.gpaste
    networkmanagerapplet
    pinentry
    pommed_light
    rofi
    rofi-pass
    udiskie-appindicator
    volnoti
    xclip
    xdotool
    # haskellPackages.status-notifier-item
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
    stack2nix
    stack

    # Tools
    binutils
    gcc
    gitFull
    git-sync
    gnumake
    gnupg
    htop
    inotify-tools
    ncdu
    pass
    python-with-my-packages
    # qttools
    rcm
    silver-searcher
    stow
    tmux
    wget
    wmctrl
    zsh

    # Nix
    nix-prefetch-git

    # Miscellaneous
    android-udev-rules
    librsvg
    transmission-gtk
  ];

  environment.variables = {
    GDK_PIXBUF_MODULE_FILE = "${pkgs.librsvg.out}/lib/gdk-pixbuf-2.0/2.10.0/loaders.cache";
  };

  programs.gnupg.agent = { enable = true; enableSSHSupport = true; };
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
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  services.xserver = {
    exportConfiguration = true;
    enable = true;
    layout = "us";
    desktopManager = {
      gnome3.enable = true;
      default = "none";
    };
    windowManager = {
      default = "xmonad";
      i3.enable = true;
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
  users.extraUsers.imalison = {
    name = "imalison";
    group = "users";
    isNormalUser = true;
    extraGroups = [
      "wheel" "disk" "audio" "video"
      "networkmanager" "systemd-journal"
    ];
    createHome = true;
    uid = 1000;
    home = "/home/imalison";
    shell = pkgs.zsh;
  };

  system.stateVersion = "18.03";
}
