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
  # clipit-master = pkgs.clipt.overrideAttrs (oldAttrs: rec {
  #   src = fetchFromGitHub {
  #     owner = "shantzu";
  #     repo = "ClipIt";
  #     rev = "eb9adaf2b5fd65aac1e83d6544b9076aae6af5b7";
  #     sha256 = "01if8y93wa0mwbkzkzx2v1vqh47zlz4k1dysl6yh5rmppd1psknz";
  #   };
  # });
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
    google-chrome
    firefox
    hexchat
    kodi
    vlc
    xfce.thunar
    spotify
    termite
    rxvt_unicode
    emacs

    # Appearance
    numix-icon-theme-circle
    gnome3.adwaita-icon-theme
    hicolor-icon-theme

    # Desktop
    autorandr
    clipit
    compton
    feh
    networkmanagerapplet
    pinentry
    pommed_light
    rofi
    rofi-pass
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
    stack2nix
    stack

    # Tools
    binutils
    gcc
    gitFull
    gnumake
    gnupg
    htop
    inotify-tools
    ncdu
    pass
    python-with-my-packages
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

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  environment.loginShellInit = ". ~/.lib/nix_login.sh";

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
