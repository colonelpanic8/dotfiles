{ pkgs, ... }:
{
  # Automatic garbage collection of old home-manager generations
  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 7d";
  };

  xdg.configFile."greenclip.toml".text = ''
    [greenclip]
      history_file = "~/.cache/greenclip.history"
      max_history_length = 50
      max_selection_size_bytes = 0
      trim_space_from_selection = true
      use_primary_selection_as_input = false
      blacklisted_applications = []
      enable_image_support = true
      image_cache_directory = "~/.cache/greenclip"
      static_history = []
  '';

  xsession = {
    enable = true;
    preferStatusNotifierItems = true;
    importedVariables = [ "GDK_PIXBUF_ICON_LOADER" ];
  };

  home.keyboard = null;
  home.emptyActivationPath = false;
  programs.home-manager.enable = true;

  programs.ssh = {
    enable = true;
    enableDefaultConfig = false;
    matchBlocks = {
      "*" = {
        forwardAgent = true;
        addKeysToAgent = "no";
        compression = false;
        serverAliveInterval = 0;
        serverAliveCountMax = 3;
        hashKnownHosts = false;
        userKnownHostsFile = "~/.ssh/known_hosts";
        controlMaster = "no";
        controlPath = "~/.ssh/master-%r@%n:%p";
        controlPersist = "no";
      };
    };
  };

  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 8 * 60 * 60;
    maxCacheTtl = 8 * 60 * 60;
    enableSshSupport = true;
    extraConfig = ''
      allow-emacs-pinentry
      allow-loopback-pinentry
    '';
  };

  gtk = {
    enable = true;
    iconTheme = {
      package = pkgs.numix-icon-theme-circle;
      name = "Numix-Circle";
    };

    font = {
      package = pkgs.noto-fonts-color-emoji;
      name = "Noto Sans";
      size = 10;
    };

    gtk3.extraConfig = {
      gtk-application-prefer-dark-theme = true;
      gtk-button-images = 1;
      gtk-cursor-theme-name = "breeze_cursors";
      gtk-cursor-theme-size = 24;
      gtk-decoration-layout = "icon:minimize,maximize,close";
      gtk-enable-animations = true;
      gtk-enable-event-sounds = 1;
      gtk-enable-input-feedback-sounds = 1;
      gtk-fallback-icon-theme = "gnome";
      gtk-key-theme-name = "Emacs";
      gtk-menu-images = 1;
      gtk-modules = "colorreload-gtk-module";
      gtk-primary-button-warps-slider = false;
      gtk-theme-name = "Arc";
      gtk-toolbar-icon-size = "GTK_ICON_SIZE_LARGE_TOOLBAR";
      gtk-toolbar-style = "GTK_TOOLBAR_BOTH_HORIZ";
      gtk-xft-antialias = 1;
      gtk-xft-dpi = 98304;
      gtk-xft-hinting = 1;
      gtk-xft-hintstyle = "hintslight";
      gtk-xft-rgba = "rgb";
    };
  };
}
