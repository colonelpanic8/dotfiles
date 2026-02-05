{ pkgs, ... }: {
  home-manager.users.imalison = {
    imports = [
      ./emacs.nix
    ];

    xdg.desktopEntries.google-chrome-devtools = {
      name = "Google Chrome (DevTools)";
      genericName = "Web Browser";
      comment = "Access the Internet";
      icon = "google-chrome";
      terminal = false;
      type = "Application";
      categories = [ "Network" "WebBrowser" ];
      mimeType = [
        "application/pdf"
        "application/rdf+xml"
        "application/rss+xml"
        "application/xhtml+xml"
        "application/xhtml_xml"
        "application/xml"
        "image/gif"
        "image/jpeg"
        "image/png"
        "image/webp"
        "text/html"
        "text/xml"
        "x-scheme-handler/http"
        "x-scheme-handler/https"
        "x-scheme-handler/google-chrome"
      ];
      exec = "${pkgs.google-chrome}/bin/google-chrome-stable --remote-debugging-port=46649 --remote-allow-origins=http://127.0.0.1,http://localhost %U";
      actions = {
        new-window = {
          name = "New Window";
          exec = "${pkgs.google-chrome}/bin/google-chrome-stable --remote-debugging-port=46649 --remote-allow-origins=http://127.0.0.1,http://localhost";
        };
        new-private-window = {
          name = "New Incognito Window";
          exec = "${pkgs.google-chrome}/bin/google-chrome-stable --remote-debugging-port=46649 --remote-allow-origins=http://127.0.0.1,http://localhost --incognito";
        };
      };
    };
  };
}
