{ pkgs, ... }: {
  xsession = {
    enable = true;
    preferStatusNotifierItems = true;
    windowManager.command = "${pkgs.haskellPackages.imalison-xmonad}/bin/imalison-xmonad";
    profileExtra = ''
      export ROFI_SYSTEMD_TERM="alacritty -e"
    '';
  };

  home.emptyActivationPath = false;
  programs.home-manager.enable = true;

  programs.ssh = {
    forwardAgent = true;
  };

  # programs.zsh = {
  #   enable = true;
  # };

  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 8 * 60 * 60;
    maxCacheTtl = 8 * 60 * 60;
    enableSshSupport = true;
  };

  services.picom = {
    enable = true;
  };

  services.blueman-applet = {
    enable = true;
  };

  services.taffybar = {
    enable = true;
    package = pkgs.haskellPackages.imalison-taffybar;
  };

  # notifyosd
  # skippyxd
  # volnoti

  services.kdeconnect = {
    enable = true;
    indicator = true;
  };

  services.network-manager-applet.enable = true;

  services.udiskie = {
    enable = true;
    tray = "always";
  };

  services.status-notifier-watcher.enable = true;

  services.random-background = {
    enable = true;
    display = "center";
    interval = "1h";
    imageDirectory = "%h/Pictures/wallpaper/use";
  };

  services.xsettingsd.enable = true;
}
