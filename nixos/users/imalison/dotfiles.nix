{
  config,
  lib,
  pkgs,
  ...
}: let
  session = import ../../session-variables.nix;
in {
  config = lib.mkIf config.myUsers.imalison.has.dotfiles {
    home-manager.users.imalison = {
      imports = [
        ../../emacs.nix
        ../../dotfiles-links.nix
      ];

      programs.git = {
        signing.format = "openpgp";
        settings = {
          user = {
            name = "Ivan Malison";
            email = "IvanMalison@gmail.com";
          };
          safe.directory = ["/shared/dean/railbird"];
        };
        includes =
          map (gitdir: {
            condition = "gitdir:${gitdir}";
            path = "~/.gitconfig.org-agenda-api";
          }) [
            "~/projects/org-agenda-api/"
            "~/projects/dotfiles/org-agenda-api/"
            "**/runtime/data/org/"
          ];
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
          ExecStart = "${pkgs.bash}/bin/bash ${../../../dotfiles/lib/bin/start-hyprpaper}";
          ExecStartPost = "${pkgs.bash}/bin/bash ${../../../dotfiles/lib/bin/set-hyprpaper-wallpaper}";
          Restart = "on-failure";
          RestartPreventExitStatus = 75;
          RestartSec = 1;
        };

        Install = {
          WantedBy = ["hyprland-session.target"];
        };
      };
    };
  };
}
