{ pkgs, ... }: {
  home-manager.users.imalison = {
    imports = [
      ./emacs.nix
      ./dotfiles-links.nix
    ];

    programs.git.enable = true;
    programs.gh = {
      enable = true;
      settings.git_protocol = "ssh";
    };

    # Hyprland config starts this target on login (see `dotfiles/config/hypr/hyprland.conf`).
    systemd.user.targets.hyprland-session = {
      Unit = {
        Description = "Hyprland session (custom)";
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
        WantedBy = [ "timers.target" ];
      };
    };

    systemd.user.services.hyprpaper = let
      wallpaperDir = "/var/lib/syncthing/sync/Wallpaper";
      waitForWayland = pkgs.writeShellScript "wait-for-wayland" ''
        # Hyprpaper needs a Wayland socket. systemd "Condition*" checks are
        # brittle here (they skip the unit entirely, with no retry) and also
        # don't support expanding $WAYLAND_DISPLAY in paths.
        for _ in {1..50}; do
          if [ -n "$WAYLAND_DISPLAY" ] && [ -n "$XDG_RUNTIME_DIR" ] && [ -S "$XDG_RUNTIME_DIR/$WAYLAND_DISPLAY" ]; then
            exit 0
          fi
          ${pkgs.coreutils}/bin/sleep 0.1
        done

        echo "Wayland socket not ready: WAYLAND_DISPLAY=$WAYLAND_DISPLAY XDG_RUNTIME_DIR=$XDG_RUNTIME_DIR" >&2
        exit 1
      '';
      setWallpaper = pkgs.writeShellScript "set-hyprpaper-wallpaper" ''
        # hyprpaper 0.8.x doesn't seem to apply `wallpaper = ...` entries from
        # its config on startup reliably. Explicitly set the wallpaper via IPC
        # once the hyprpaper socket is ready.
        #
        # NOTE: hyprctl hyprpaper currently ignores `--instance` and uses
        # $HYPRLAND_INSTANCE_SIGNATURE to find hyprpaper's socket, so we rely on
        # the environment imported by Hyprland on login.
        set -u

        runtimeDir="''${XDG_RUNTIME_DIR:-}"
        sig="''${HYPRLAND_INSTANCE_SIGNATURE:-}"
        sockPath=""
        if [ -n "$runtimeDir" ] && [ -n "$sig" ]; then
          sockPath="$runtimeDir/hypr/$sig/.hyprpaper.sock"
        fi

        # Don't fail the service if env isn't available yet.
        if [ -z "$sockPath" ]; then
          echo "set-hyprpaper-wallpaper: missing XDG_RUNTIME_DIR/HYPRLAND_INSTANCE_SIGNATURE" >&2
          exit 0
        fi

        for _ in {1..50}; do
          if [ -S "$sockPath" ]; then
            break
          fi
          ${pkgs.coreutils}/bin/sleep 0.1
        done

        if [ ! -S "$sockPath" ]; then
          echo "set-hyprpaper-wallpaper: hyprpaper socket not ready at $sockPath" >&2
          exit 0
        fi

        if [ ! -d "${wallpaperDir}" ]; then
          echo "set-hyprpaper-wallpaper: wallpaper directory missing: ${wallpaperDir}" >&2
          exit 0
        fi

        wallpaper="$(${pkgs.findutils}/bin/find "${wallpaperDir}" -type f -regextype posix-extended -iregex '.*\.(png|jpe?g|webp)$' -print0 | ${pkgs.coreutils}/bin/shuf -z -n 1 | ${pkgs.coreutils}/bin/tr -d '\0')"

        if [ -z "$wallpaper" ]; then
          echo "set-hyprpaper-wallpaper: no wallpapers found in ${wallpaperDir}" >&2
          exit 0
        fi

        # Apply to all current monitors.
        mons=$(/run/current-system/sw/bin/hyprctl -j monitors 2>/dev/null | ${pkgs.jq}/bin/jq -r '.[].name' 2>/dev/null || true)
        if [ -z "$mons" ]; then
          # Fallback to parsing non-JSON output.
          mons=$(/run/current-system/sw/bin/hyprctl monitors 2>/dev/null | ${pkgs.gnugrep}/bin/grep -oE '^Monitor [^ ]+' | ${pkgs.coreutils}/bin/cut -d' ' -f2 || true)
        fi

        for mon in $mons; do
          # The socket file can exist before hyprpaper is actually ready to
          # accept connections, so retry briefly.
          for _ in {1..50}; do
            if /run/current-system/sw/bin/hyprctl hyprpaper wallpaper "$mon,$wallpaper" >/dev/null 2>&1; then
              break
            fi
            ${pkgs.coreutils}/bin/sleep 0.1
          done
        done

        exit 0
      '';
      hyprpaperConf = pkgs.writeText "hyprpaper.conf" ''
        ipc = true
        splash = false
      '';
    in {
      Unit = {
        Description = "Hyprpaper (managed by home-manager)";
        PartOf = [ "hyprland-session.target" ];
        After = [ "hyprland-session.target" ];
      };

      Service = {
        ExecStartPre = waitForWayland;
        ExecStart = "${pkgs.hyprpaper}/bin/hyprpaper -c ${hyprpaperConf}";
        ExecStartPost = setWallpaper;
        Restart = "on-failure";
        RestartSec = 1;
      };

      Install = {
        WantedBy = [ "hyprland-session.target" ];
      };
    };

    systemd.user.services.tailscale-systray = {
      Unit = {
        Description = "Tailscale systray";
        After = [ "graphical-session.target" "tray.target" ];
        PartOf = [ "graphical-session.target" ];
        Requires = [ "tray.target" ];
      };

      Service = {
        ExecStart = "${pkgs.tailscale}/bin/tailscale systray";
        Restart = "on-failure";
        RestartSec = 3;
      };

      Install = {
        WantedBy = [ "graphical-session.target" ];
      };
    };

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
