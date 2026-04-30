{
  config,
  pkgs,
  lib,
  nixos,
  ...
}: let
  mimeMap = desktopId: mimeTypes: lib.genAttrs mimeTypes (_: [desktopId]);
  browser = "google-chrome.desktop";
  imageViewer = "org.kde.gwenview.desktop";
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
      "image/heif"
      "image/jpeg"
      "image/jxl"
      "image/png"
      "image/svg+xml"
      "image/svg+xml-compressed"
      "image/tiff"
      "image/vnd.microsoft.icon"
      "image/webp"
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
  mimeAppsListText = let
    formatApplications = applications:
      lib.concatStringsSep "\n" (
        lib.mapAttrsToList (
          mimeType: desktopIds: "${mimeType}=${lib.concatStringsSep ";" desktopIds};"
        )
        applications
      );
  in ''
    [Added Associations]
    ${formatApplications defaultApplications}

    [Default Applications]
    ${formatApplications defaultApplications}

    [Removed Associations]
  '';
  qtctConfig = ''
    [Appearance]
    color_scheme_path=
    custom_palette=false
    icon_theme=Numix-Circle
    standard_dialogs=default
    style=Fusion

    [Fonts]
    fixed="Noto Sans,10,-1,5,50,0,0,0,0,0"
    general="Noto Sans,10,-1,5,50,0,0,0,0,0"

    [Interface]
    activate_item_on_single_click=1
    buttonbox_layout=0
    cursor_flash_time=1000
    dialog_buttons_have_icons=1
    double_click_interval=400
    gui_effects=@Invalid()
    keyboard_scheme=2
    menus_have_icons=true
    show_shortcuts_in_context_menus=true
    stylesheets=@Invalid()
    toolbutton_style=4
    underline_shortcut=1
    wheel_scroll_lines=3
  '';
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

  xdg.configFile."qt5ct/qt5ct.conf".text = qtctConfig;
  xdg.configFile."qt6ct/qt6ct.conf".text = qtctConfig;

  xdg.configFile."zellij/config.kdl".source =
    config.lib.file.mkOutOfStoreSymlink
    "${config.home.homeDirectory}/dotfiles/dotfiles/config/zellij/config.kdl";

  xdg.configFile."menus/applications.menu" = lib.mkIf nixos.config.myModules.desktop.enable {
    source = "${pkgs.kdePackages.plasma-workspace}/etc/xdg/menus/plasma-applications.menu";
  };

  xdg.configFile."kde-mimeapps.list" = lib.mkIf nixos.config.myModules.desktop.enable {
    text = mimeAppsListText;
  };

  xdg.configFile."none+xmonad-mimeapps.list" = lib.mkIf nixos.config.myModules.desktop.enable {
    text = mimeAppsListText;
  };

  xdg.configFile."xmonad-mimeapps.list" = lib.mkIf nixos.config.myModules.desktop.enable {
    text = mimeAppsListText;
  };

  xdg.configFile."hyprland-mimeapps.list" = lib.mkIf nixos.config.myModules.desktop.enable {
    text = mimeAppsListText;
  };

  xdg.dataFile."mimeapps.list" = lib.mkIf nixos.config.myModules.desktop.enable {
    text = mimeAppsListText;
  };

  xdg.dataFile."applications/kde-mimeapps.list" = lib.mkIf nixos.config.myModules.desktop.enable {
    text = mimeAppsListText;
  };

  xdg.mimeApps = lib.mkIf nixos.config.myModules.desktop.enable {
    enable = true;
    associations.added = defaultApplications;
    inherit defaultApplications;
  };

  home.activation.refreshChromeDesktopMimeCache = lib.hm.dag.entryAfter ["writeBoundary"] ''
    applications_dir="$HOME/.local/share/applications"

    if [ -d "$applications_dir" ]; then
      for desktop_file in \
        "$applications_dir/google-chrome.desktop" \
        "$applications_dir/com.google.Chrome.desktop"
      do
        if [ -f "$desktop_file" ]; then
          ${pkgs.gnused}/bin/sed -i \
            -e 's,application/pdf;,,g' \
            -e 's,image/gif;,,g' \
            -e 's,image/jpeg;,,g' \
            -e 's,image/png;,,g' \
            -e 's,image/webp;,,g' \
            "$desktop_file"
        fi
      done

      for desktop_file in "$applications_dir"/okular*.desktop "$applications_dir"/vlc*.desktop; do
        if [ -f "$desktop_file" ]; then
          ${pkgs.gnused}/bin/sed -i \
            -e 's,image/avif;,,g' \
            -e 's,image/bmp;,,g' \
            -e 's,image/gif;,,g' \
            -e 's,image/heic;,,g' \
            -e 's,image/heif;,,g' \
            -e 's,image/jpeg;,,g' \
            -e 's,image/jxl;,,g' \
            -e 's,image/png;,,g' \
            -e 's,image/svg+xml;,,g' \
            -e 's,image/svg+xml-compressed;,,g' \
            -e 's,image/tiff;,,g' \
            -e 's,image/vnd.microsoft.icon;,,g' \
            -e 's,image/webp;,,g' \
            "$desktop_file"
        fi
      done

      ${pkgs.desktop-file-utils}/bin/update-desktop-database "$applications_dir" >/dev/null 2>&1 || true
    fi
  '';

  home.activation.refreshKdeServiceCache = lib.hm.dag.entryAfter ["refreshChromeDesktopMimeCache"] ''
    ${pkgs.kdePackages.kservice}/bin/kbuildsycoca6 --noincremental >/dev/null 2>&1 || true
  '';

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

    gtk3.extraCss = ''
      @binding-set gtk-emacs-text-entry
      {
        bind "<ctrl>b" { "move-cursor" (logical-positions, -1, 0) };
        bind "<shift><ctrl>b" { "move-cursor" (logical-positions, -1, 1) };
        bind "<ctrl>f" { "move-cursor" (logical-positions, 1, 0) };
        bind "<shift><ctrl>f" { "move-cursor" (logical-positions, 1, 1) };

        bind "<alt>b" { "move-cursor" (words, -1, 0) };
        bind "<shift><alt>b" { "move-cursor" (words, -1, 1) };
        bind "<alt>f" { "move-cursor" (words, 1, 0) };
        bind "<shift><alt>f" { "move-cursor" (words, 1, 1) };

        bind "<ctrl>a" { "move-cursor" (paragraph-ends, -1, 0) };
        bind "<shift><ctrl>a" { "move-cursor" (paragraph-ends, -1, 1) };
        bind "<ctrl>e" { "move-cursor" (paragraph-ends, 1, 0) };
        bind "<shift><ctrl>e" { "move-cursor" (paragraph-ends, 1, 1) };

        bind "<ctrl>w" { "cut-clipboard" () };
        bind "<ctrl>y" { "paste-clipboard" () };

        bind "<ctrl>d" { "delete-from-cursor" (chars, 1) };
        bind "<alt>d" { "delete-from-cursor" (word-ends, 1) };
        bind "<alt>BackSpace" { "delete-from-cursor" (word-ends, -1) };
        bind "<ctrl>k" { "delete-from-cursor" (paragraph-ends, 1) };

        bind "<alt>space" { "delete-from-cursor" (whitespace, 1)
                            "insert-at-cursor" (" ") };
        bind "<alt>KP_Space" { "delete-from-cursor" (whitespace, 1)
                               "insert-at-cursor" (" ")  };
        /*
         * Some non-Emacs keybindings people are attached to
         */
        bind "<ctrl>u" { "move-cursor" (paragraph-ends, -1, 0)
                         "delete-from-cursor" (paragraph-ends, 1) };

        bind "<ctrl>h" { "delete-from-cursor" (chars, -1) };
        bind "<ctrl>w" { "delete-from-cursor" (word-ends, -1) };
      }

      /*
       * Bindings for GtkTextView
       */
      @binding-set gtk-emacs-text-view
      {
        bind "<ctrl>p" { "move-cursor" (display-lines, -1, 0) };
        bind "<shift><ctrl>p" { "move-cursor" (display-lines, -1, 1) };
        bind "<ctrl>n" { "move-cursor" (display-lines, 1, 0) };
        bind "<shift><ctrl>n" { "move-cursor" (display-lines, 1, 1) };

        bind "<ctrl>space" { "set-anchor" () };
        bind "<ctrl>KP_Space" { "set-anchor" () };
      }

      /*
       * Bindings for GtkTreeView
       */
      @binding-set gtk-emacs-tree-view
      {
        bind "<ctrl>s" { "start-interactive-search" () };
        bind "<ctrl>f" { "move-cursor" (logical-positions, 1) };
        bind "<ctrl>b" { "move-cursor" (logical-positions, -1) };
      }

      /*
       * Bindings for menus
       */
      @binding-set gtk-emacs-menu
      {
        bind "<ctrl>n" { "move-current" (next) };
        bind "<ctrl>p" { "move-current" (prev) };
        bind "<ctrl>f" { "move-current" (child) };
        bind "<ctrl>b" { "move-current" (parent) };
      }

      entry {
        -gtk-key-bindings: gtk-emacs-text-entry;
      }

      textview {
        -gtk-key-bindings: gtk-emacs-text-entry, gtk-emacs-text-view;
      }

      treeview {
        -gtk-key-bindings: gtk-emacs-tree-view;
      }

      GtkMenuShell {
        -gtk-key-bindings: gtk-emacs-menu;
      }
      @import 'colors.css';
    '';
  };
}
