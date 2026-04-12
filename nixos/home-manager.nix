{
  config,
  pkgs,
  lib,
  nixos,
  ...
}: let
  mimeMap = desktopId: mimeTypes: lib.genAttrs mimeTypes (_: [desktopId]);
in {
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

  xdg.mimeApps = lib.mkIf nixos.config.myModules.desktop.enable (
    let
      browser = "google-chrome.desktop";
      imageViewer = "org.gnome.Loupe.desktop";
      fallbackImageViewer = "okularApplication_kimgio.desktop";
      pdfViewer = "okularApplication_pdf.desktop";
      comicViewer = "okularApplication_comicbook.desktop";
      djvuViewer = "okularApplication_djvu.desktop";
      ebookViewer = "okularApplication_epub.desktop";
      mobiViewer = "okularApplication_mobi.desktop";
      xpsViewer = "okularApplication_xps.desktop";
      mediaPlayer = "vlc.desktop";
      archiveManager = "org.gnome.FileRoller.desktop";
      fileManager = "thunar.desktop";
      wordProcessor = "writer.desktop";
      spreadsheet = "calc.desktop";
      presentation = "impress.desktop";

      defaultApplications =
        (mimeMap imageViewer [
          "image/avif"
          "image/bmp"
          "image/gif"
          "image/heic"
          "image/jpeg"
          "image/jxl"
          "image/png"
          "image/svg+xml"
          "image/svg+xml-compressed"
          "image/tiff"
          "image/vnd.microsoft.icon"
          "image/webp"
        ])
        // (mimeMap fallbackImageViewer [
          "image/heif"
        ])
        // (mimeMap pdfViewer [
          "application/pdf"
          "application/x-bzpdf"
          "application/x-gzpdf"
        ])
        // (mimeMap comicViewer [
          "application/x-cb7"
          "application/x-cbr"
          "application/x-cbt"
          "application/x-cbz"
        ])
        // (mimeMap djvuViewer [
          "image/vnd.djvu"
        ])
        // (mimeMap ebookViewer [
          "application/epub+zip"
        ])
        // (mimeMap mobiViewer [
          "application/x-mobipocket-ebook"
        ])
        // (mimeMap xpsViewer [
          "application/oxps"
          "application/vnd.ms-xpsdocument"
        ])
        // (mimeMap mediaPlayer [
          "application/ogg"
          "audio/flac"
          "audio/mp4"
          "audio/mpeg"
          "audio/ogg"
          "audio/opus"
          "audio/webm"
          "audio/wav"
          "audio/x-flac"
          "audio/x-wav"
          "video/mp4"
          "video/ogg"
          "video/quicktime"
          "video/webm"
          "video/x-matroska"
          "video/x-msvideo"
        ])
        // (mimeMap archiveManager [
          "application/bzip2"
          "application/gzip"
          "application/vnd.rar"
          "application/x-7z-compressed"
          "application/x-bzip"
          "application/x-compressed-tar"
          "application/x-gzip"
          "application/x-rar"
          "application/x-rar-compressed"
          "application/x-tar"
          "application/x-xz"
          "application/x-zip-compressed"
          "application/zip"
          "application/zstd"
        ])
        // (mimeMap wordProcessor [
          "application/msword"
          "application/rtf"
          "application/vnd.ms-word"
          "application/vnd.oasis.opendocument.text"
          "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
        ])
        // (mimeMap spreadsheet [
          "application/vnd.ms-excel"
          "application/vnd.oasis.opendocument.spreadsheet"
          "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
          "text/csv"
          "text/tab-separated-values"
        ])
        // (mimeMap presentation [
          "application/mspowerpoint"
          "application/vnd.ms-powerpoint"
          "application/vnd.oasis.opendocument.presentation"
          "application/vnd.openxmlformats-officedocument.presentationml.presentation"
          "application/vnd.openxmlformats-officedocument.presentationml.slideshow"
        ])
        // (mimeMap fileManager [
          "inode/directory"
        ])
        // (mimeMap browser [
          "application/rdf+xml"
          "application/rss+xml"
          "application/xhtml+xml"
          "application/xhtml_xml"
          "application/xml"
          "text/html"
          "text/xml"
          "x-scheme-handler/about"
          "x-scheme-handler/http"
          "x-scheme-handler/https"
          "x-scheme-handler/unknown"
        ])
        // {
          "x-scheme-handler/element" = ["element-desktop.desktop"];
          "x-scheme-handler/magnet" = ["transmission-gtk.desktop"];
        };
    in {
      enable = true;
      associations.added = defaultApplications;
      inherit defaultApplications;
    }
  );

  xsession = {
    enable = true;
    preferStatusNotifierItems = true;
    importedVariables = ["GDK_PIXBUF_ICON_LOADER"];
    profileExtra = ''
      systemctl --user set-environment IMALISON_SESSION_TYPE=x11
    '';
  };

  home.keyboard = null;
  home.emptyActivationPath = false;
  programs.home-manager.enable = true;

  programs.ssh = {
    enable = true;
    enableDefaultConfig = false;
    extraConfig = ''
      Include /home/imalison/config/dotfiles/ssh/config
    '';
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

  home.file.".ssh/config".force = true;

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

    theme = {
      package = pkgs.arc-theme;
      name = "Arc";
    };

    gtk4.theme = config.gtk.theme;

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
