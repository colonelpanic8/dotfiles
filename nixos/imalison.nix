{
  config,
  pkgs,
  ...
}: let
  session = import ./session-variables.nix;
  dotfilesRoot = config.dotfiles-worktree;
  cargoSweepRustTargets = pkgs.writeShellApplication {
    name = "cargo-sweep-rust-targets";
    runtimeInputs = [pkgs.cargo-sweep];
    text = ''
      for root in /home/imalison/Projects /home/imalison/org ${dotfilesRoot}; do
        if [[ -d "$root" ]]; then
          cargo-sweep sweep -r --hidden --maxsize 15GB "$root"
        fi
      done
    '';
  };
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
        Description = "Sweep Rust target directories daily";
      };

      Timer = {
        OnCalendar = "daily";
        Persistent = true;
        RandomizedDelaySec = "2h";
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
