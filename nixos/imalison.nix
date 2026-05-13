{pkgs, ...}: let
  session = import ./session-variables.nix;
in {
  home-manager.users.imalison = {
    imports = [
      ./emacs.nix
      ./dotfiles-links.nix
    ];

    programs.git.enable = true;
    programs.git.signing.format = "openpgp";
    programs.gh = {
      enable = true;
      settings.git_protocol = "ssh";
    };

    # Hyprland config starts this target on login.
    systemd.user.targets.hyprland-session = {
      Unit = {
        Description = "Hyprland session (custom)";
        ConditionEnvironment = session.hyprland;
      };
    };

    # Rootless podman stores images/layers under ~/.local/share/containers.
    # NixOS' `virtualisation.podman.autoPrune` only affects the rootful store,
    # so we prune the per-user store with a user timer.
    systemd.user.services.podman-auto-prune = {
      Unit = {
        Description = "Podman auto prune (rootless)";
      };

      Service = {
        Type = "oneshot";
        ExecStart = "${pkgs.podman}/bin/podman system prune -a -f";
      };
    };

    systemd.user.timers.podman-auto-prune = {
      Unit = {
        Description = "Podman auto prune (rootless)";
      };

      Timer = {
        OnCalendar = "daily";
        Persistent = true;
        RandomizedDelaySec = "1h";
      };

      Install = {
        WantedBy = ["timers.target"];
      };
    };

    systemd.user.services.hyprpaper = let
      hyprpaperConf = pkgs.writeText "hyprpaper.conf" ''
        ipc = true
        splash = false
      '';
      hyprpaperPath = pkgs.lib.makeBinPath [
        pkgs.coreutils
        pkgs.findutils
        pkgs.gnugrep
        pkgs.gnused
        pkgs.hyprpaper
        pkgs.jq
      ];
    in {
      Unit = {
        Description = "Hyprpaper (managed by home-manager)";
        ConditionEnvironment = session.hyprland;
        PartOf = ["hyprland-session.target"];
        After = ["hyprland-session.target"];
      };

      Service = {
        Environment = [
          "HYPRPAPER_CONFIG=${hyprpaperConf}"
          "HYPRCTL=/run/current-system/sw/bin/hyprctl"
          "PATH=${hyprpaperPath}:/run/current-system/sw/bin"
          "WALLPAPER_DIR=/var/lib/syncthing/sync/Wallpaper/use"
        ];
        ExecStart = "${pkgs.bash}/bin/bash ${../dotfiles/lib/bin/start-hyprpaper}";
        ExecStartPost = "${pkgs.bash}/bin/bash ${../dotfiles/lib/bin/set-hyprpaper-wallpaper}";
        Restart = "on-failure";
        RestartPreventExitStatus = 75;
        RestartSec = 1;
      };

      Install = {
        WantedBy = ["hyprland-session.target"];
      };
    };

    systemd.user.services.tailscale-systray = {
      Unit = {
        Description = "Tailscale systray";
        After = ["graphical-session.target" "tray.target"];
        PartOf = ["graphical-session.target"];
        Requires = ["tray.target"];
      };

      Service = {
        ExecStart = "${pkgs.tailscale}/bin/tailscale systray";
        Restart = "on-failure";
        RestartSec = 3;
      };

      Install = {
        WantedBy = ["graphical-session.target"];
      };
    };

    xdg.desktopEntries.google-chrome-devtools = {
      name = "Google Chrome (DevTools)";
      genericName = "Web Browser";
      comment = "Access the Internet";
      icon = "google-chrome";
      terminal = false;
      type = "Application";
      categories = ["Network" "WebBrowser"];
      mimeType = [
        "application/rdf+xml"
        "application/rss+xml"
        "application/xhtml+xml"
        "application/xhtml_xml"
        "application/xml"
        "text/html"
        "text/xml"
        "x-scheme-handler/http"
        "x-scheme-handler/https"
        "x-scheme-handler/google-chrome"
      ];
      exec = "${pkgs.google-chrome}/bin/google-chrome-stable --user-data-dir=/home/imalison/.cache/google-chrome-devtools --remote-debugging-port=46649 --remote-allow-origins=http://127.0.0.1,http://localhost %U";
      actions = {
        new-window = {
          name = "New Window";
          exec = "${pkgs.google-chrome}/bin/google-chrome-stable --user-data-dir=/home/imalison/.cache/google-chrome-devtools --remote-debugging-port=46649 --remote-allow-origins=http://127.0.0.1,http://localhost";
        };
        new-private-window = {
          name = "New Incognito Window";
          exec = "${pkgs.google-chrome}/bin/google-chrome-stable --user-data-dir=/home/imalison/.cache/google-chrome-devtools --remote-debugging-port=46649 --remote-allow-origins=http://127.0.0.1,http://localhost --incognito";
        };
      };
    };
  };
}
