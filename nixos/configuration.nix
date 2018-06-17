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
in
{
  boot.loader.systemd-boot.enable = true;
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
    sddm-kcm
    networkmanagerapplet
    pinentry
    pommed_light
    rofi
    rofi-pass
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

    # Miscellaneous
    librsvg
  ];

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
    videoDrivers = [ "nvidia" ];
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
