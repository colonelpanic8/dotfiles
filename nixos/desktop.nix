{ inputs, config, pkgs, lib, makeEnable, ... }:
makeEnable config "myModules.desktop" true {
  services.greenclip.enable = true;
  imports = [
    ./fonts.nix
    ./hyprland.nix
    ./keyd.nix
  ];

  assertions = [
    {
      assertion = config.myModules.taffybar.enable != config.myModules.waybar.enable;
      message = "Enable exactly one of myModules.taffybar or myModules.waybar.";
    }
  ];

  myModules.taffybar.enable = lib.mkDefault config.myModules.xmonad.enable;
  myModules.waybar.enable = lib.mkDefault (!config.myModules.xmonad.enable);

  services.xserver = {
    exportConfiguration = true;
    enable = true;
    displayManager = {
      sessionCommands = ''
        systemctl --user import-environment GDK_PIXBUF_MODULE_FILE DBUS_SESSION_BUS_ADDRESS PATH
      '';
      setupCommands = ''
        autorandr -c
        systemctl restart autorandr.service
      '';
    };
  };

  services.autorandr = {
    enable = true;
  };

  # This is for the benefit of VSCODE running natively in wayland
  environment.sessionVariables.NIXOS_OZONE_WL = "1";

  services.gnome.at-spi2-core.enable = true;

  services.gnome.gnome-keyring.enable = true;

  # Visual notification manager

  environment.systemPackages = with pkgs; [
    # Appearance
    adwaita-icon-theme
    hicolor-icon-theme
    # libsForQt5.breeze-gtk
    # materia-theme
    numix-icon-theme-circle
    papirus-icon-theme

    # XOrg
    autorandr
    keyd
    wmctrl
    xclip
    xdotool
    xev
    xwininfo
    xsettingsd

    # Desktop
    alacritty
    ghostty
    blueman
    # clipit
    d-spy
    kdePackages.dolphin

    feh
    gthumb
    firefox
    cheese
    kdePackages.kleopatra
    libnotify
    libreoffice
    lxappearance
    lxqt.lxqt-powermanagement
    networkmanagerapplet
    kdePackages.okular
    pinentry-gnome3
    # mission-center
    quassel
    remmina
    rofi
    wofi
    rofi-pass
    rofi-systemd
    simplescreenrecorder
    skippy-xd
    synergy
    transmission_4-gtk
    vlc
    thunar

    # Audio
    picard
    pavucontrol
    playerctl
    pulsemixer
    espeak

    #
    brightnessctl

    # Visualization
    graphviz
    nodePackages.mermaid-cli
  ] ++ (if pkgs.stdenv.hostPlatform.system == "x86_64-linux" then with pkgs; [
    google-chrome
    pommed_light
    slack
    spicetify-cli
    spotify
    tor-browser
    vscode
    zulip
  ] else []);
}
