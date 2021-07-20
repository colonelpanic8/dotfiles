{ pkgs, config, ... }: {
  xsession = {
    enable = true;
    preferStatusNotifierItems = true;
    importedVariables = [ "GDK_PIXBUF_ICON_LOADER" "PATH" ];
    profileExtra = ''
      export ROFI_SYSTEMD_TERM="alacritty -e"
      . "$HOME/.lib/login.sh"
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

  services.blueman-applet = {
    enable = false;
  };

  services.taffybar = {
    enable = true;
    package = pkgs.haskellPackages.imalison-taffybar;
  };

  services.notify-osd = {
    enable = true;
    package = pkgs.notify-osd-customizable;
  };
  # skippyxd

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

  services.volnoti.enable = true;

  services.git-sync = {
    enable = true;
    repositories = [
      {
        name = "config";
        path = "/home/imalison/config";
        uri = "git@github.com:IvanMalison/config.git";
      }
      {
        name = "org";
        path = "/home/imalison/org";
        uri = "git@github.com:IvanMalison/org.git";
      }
      {
        name = "password-store";
        path = "/home/imalison/.password-store";
        uri = "git@github.com:IvanMalison/.password-store.git";
      }
    ];
  };

  systemd.user.services.picom = {
    Unit = {
      Description = "Picom X11 compositor";
      After = [ "graphical-session-pre.target" ];
      PartOf = [ "graphical-session.target" ];
    };

    Install = { WantedBy = [ "graphical-session.target" ]; };

    Service = {
      # Temporarily fixes corrupt colours with Mesa 18.
      Environment = [ "allow_rgb10_configs=false" ];
      ExecStart = "${pkgs.picom}/bin/picom";
      Restart = "always";
      RestartSec = 3;
    };
  };
}
