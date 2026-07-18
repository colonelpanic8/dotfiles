{
  config,
  pkgs,
  ...
}: let
  session = import ./session-variables.nix;
  dotfilesRoot = config.dotfiles-worktree;
  cargoSweepRustTargets = pkgs.writeShellApplication {
    name = "cargo-sweep-rust-targets";
    runtimeInputs = [pkgs.cargo-sweep pkgs.findutils];
    text = ''
      build_root="$HOME/.cargo/build"
      for root in /home/imalison/Projects /home/imalison/org ${dotfilesRoot} "$build_root"; do
        if [[ -d "$root" ]]; then
          cargo-sweep sweep -r --hidden --time 2 "$root"
        fi
      done
      # Drop centralized build dirs (build_root/<xx>/<hash>) whose workspace is
      # gone or idle: cargo-sweep only removes artifacts inside them, so remove
      # any hash dir with nothing touched in the last 2 days, then empty shells.
      if [[ -d "$build_root" ]]; then
        for dir in "$build_root"/*/*/; do
          [[ -d "$dir" ]] || continue
          if [[ -z "$(find "$dir" -newermt '2 days ago' -print -quit)" ]]; then
            rm -rf "$dir"
          fi
        done
        find "$build_root" -mindepth 1 -type d -empty -delete
      fi
    '';
  };
in {
  home-manager.users.imalison = {
    imports = [
      ./emacs.nix
      ./dotfiles-links.nix
    ];

    # The GTK file-chooser right-click context menu (Show Hidden Files, etc.)
    # is unreliable under Hyprland/wlroots (upstream popup-focus bug,
    # hyprwm/Hyprland#6426), so set these toggles as defaults instead of
    # relying on the broken menu. Ctrl+H still toggles hidden files live.
    dconf.settings."org/gtk/settings/file-chooser" = {
      show-hidden = true;
      sort-directories-first = true;
    };

    # Centralize cargo's intermediate build artifacts (deps, incremental
    # caches, fingerprints — the bulk of target-dir bloat) under
    # ~/.cargo/build/<workspace-path-hash>, so deleting a worktree doesn't
    # strand gigabytes and the sweep timer has one place to prune. Final
    # artifacts still land in each project's target/.
    home.file.".cargo/config.toml".text = ''
      [build]
      build-dir = "{cargo-cache-home}/build/{workspace-path-hash}"
    '';

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

    systemd.user.services.cargo-sweep-rust-targets = {
      Unit = {
        Description = "Sweep Rust target directories";
      };

      Service = {
        Type = "oneshot";
        ExecStart = "${cargoSweepRustTargets}/bin/cargo-sweep-rust-targets";
      };
    };

    systemd.user.timers.cargo-sweep-rust-targets = {
      Unit = {
        Description = "Sweep Rust target directories every 6 hours";
      };

      Timer = {
        OnCalendar = "00/6:00";
        Persistent = true;
        RandomizedDelaySec = "30m";
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
  };
}
