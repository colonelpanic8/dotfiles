{
  config,
  inputs,
  lib,
  pkgs,
  makeEnable,
  ...
}: let
  system = pkgs.stdenv.hostPlatform.system;
  taffybarPackage = inputs.imalison-taffybar.defaultPackage.${system};
  taffybarStart = pkgs.writeShellScript "taffybar-start" ''
    runtime_dir="''${XDG_RUNTIME_DIR:-/run/user/$(${pkgs.coreutils}/bin/id -u)}"

    find_wayland_socket() {
      if [ -n "''${WAYLAND_DISPLAY:-}" ] && [ -S "$runtime_dir/$WAYLAND_DISPLAY" ]; then
        printf '%s\n' "$WAYLAND_DISPLAY"
        return 0
      fi

      for socket in "$runtime_dir"/wayland-*; do
        case "$socket" in
          *.lock) continue ;;
        esac

        if [ -S "$socket" ]; then
          printf '%s\n' "''${socket##*/}"
          return 0
        fi
      done

      return 1
    }

    x11_socket_ready() {
      display_number="''${DISPLAY#:}"
      display_number="''${display_number%%.*}"
      [ -n "$display_number" ] && [ -S "/tmp/.X11-unix/X$display_number" ]
    }

    current_desktop="''${XDG_CURRENT_DESKTOP:-}"
    desktop_session="''${DESKTOP_SESSION:-}"
    window_manager="''${IMALISON_WINDOW_MANAGER:-}"
    is_hyprland=0
    case "''${current_desktop}:''${desktop_session}:''${window_manager}" in
      *Hyprland*|*hyprland*) is_hyprland=1 ;;
    esac

    if [ "$is_hyprland" = 1 ] && [ -z "''${XDG_SESSION_TYPE:-}" ]; then
      export XDG_SESSION_TYPE=wayland
    fi

    if [ "''${XDG_SESSION_TYPE:-}" = "wayland" ] || [ -n "''${WAYLAND_DISPLAY:-}" ] || [ "$is_hyprland" = 1 ]; then
      if [ -z "''${WAYLAND_DISPLAY:-}" ] || [ ! -S "$runtime_dir/$WAYLAND_DISPLAY" ]; then
        if socket_name="$(find_wayland_socket)"; then
          echo "taffybar-start: correcting WAYLAND_DISPLAY=''${WAYLAND_DISPLAY:-<unset>} to $socket_name" >&2
          export WAYLAND_DISPLAY="$socket_name"
        fi
      fi
    fi

    if [ -n "''${DISPLAY:-}" ] && [ "''${XDG_SESSION_TYPE:-}" != "wayland" ] && x11_socket_ready; then
      unset WAYLAND_DISPLAY
      unset HYPRLAND_INSTANCE_SIGNATURE
      exec ${taffybarPackage}/bin/taffybar "$@"
    fi

    if [ -z "''${WAYLAND_DISPLAY:-}" ] || [ ! -S "$runtime_dir/$WAYLAND_DISPLAY" ]; then
      if socket_name="$(find_wayland_socket)"; then
        echo "taffybar-start: correcting WAYLAND_DISPLAY=''${WAYLAND_DISPLAY:-<unset>} to $socket_name" >&2
        export WAYLAND_DISPLAY="$socket_name"
      fi
    fi

    if [ -n "''${HYPRLAND_INSTANCE_SIGNATURE:-}" ] && [ ! -S "$runtime_dir/hypr/$HYPRLAND_INSTANCE_SIGNATURE/.socket.sock" ]; then
      echo "taffybar-start: unsetting stale HYPRLAND_INSTANCE_SIGNATURE=$HYPRLAND_INSTANCE_SIGNATURE" >&2
      unset HYPRLAND_INSTANCE_SIGNATURE
    fi

    if [ "$is_hyprland" = 1 ]; then
      if [ -z "''${HYPRLAND_INSTANCE_SIGNATURE:-}" ]; then
        for socket in "$runtime_dir"/hypr/*/.socket.sock; do
          if [ -S "$socket" ]; then
            socket_dir="''${socket%/.socket.sock}"
            signature="''${socket_dir##*/}"
            echo "taffybar-start: correcting HYPRLAND_INSTANCE_SIGNATURE=''${HYPRLAND_INSTANCE_SIGNATURE:-<unset>} to $signature" >&2
            export HYPRLAND_INSTANCE_SIGNATURE="$signature"
            break
          fi
        done
      fi
    fi

    exec ${taffybarPackage}/bin/taffybar "$@"
  '';
  waitForGraphicalSocket = pkgs.writeShellScript "wait-for-taffybar-display" ''
    runtime_dir="''${XDG_RUNTIME_DIR:-/run/user/$(${pkgs.coreutils}/bin/id -u)}"

    find_wayland_socket() {
      if [ -n "''${WAYLAND_DISPLAY:-}" ] && [ -S "$runtime_dir/$WAYLAND_DISPLAY" ]; then
        return 0
      fi

      for socket in "$runtime_dir"/wayland-*; do
        case "$socket" in
          *.lock) continue ;;
        esac

        if [ -S "$socket" ]; then
          return 0
        fi
      done

      return 1
    }

    x11_socket_ready() {
      display_number="''${DISPLAY#:}"
      display_number="''${display_number%%.*}"
      [ -n "$display_number" ] && [ -S "/tmp/.X11-unix/X$display_number" ]
    }

    for _ in $(${pkgs.coreutils}/bin/seq 1 50); do
      current_desktop="''${XDG_CURRENT_DESKTOP:-}"
      desktop_session="''${DESKTOP_SESSION:-}"
      window_manager="''${IMALISON_WINDOW_MANAGER:-}"

      case "''${current_desktop}:''${desktop_session}:''${window_manager}" in
        *Hyprland*|*hyprland*) find_wayland_socket && exit 0 ;;
      esac

      if [ "''${XDG_SESSION_TYPE:-}" = "wayland" ] || [ -n "''${WAYLAND_DISPLAY:-}" ]; then
        find_wayland_socket && exit 0
      elif [ -n "''${DISPLAY:-}" ]; then
        x11_socket_ready && exit 0
        find_wayland_socket && exit 0
      else
        find_wayland_socket && exit 0
      fi

      ${pkgs.coreutils}/bin/sleep 0.1
    done

    echo "taffybar: display socket not ready: XDG_SESSION_TYPE=''${XDG_SESSION_TYPE:-<unset>} WAYLAND_DISPLAY=''${WAYLAND_DISPLAY:-<unset>} DISPLAY=''${DISPLAY:-<unset>} XDG_RUNTIME_DIR=$runtime_dir" >&2
    exit 1
  '';
  skipTaffybarInOtherShells = pkgs.writeShellScript "skip-taffybar-in-other-shells" ''
    current_desktop="''${XDG_CURRENT_DESKTOP:-}"
    desktop_session="''${DESKTOP_SESSION:-}"

    case "''${current_desktop}:''${desktop_session}" in
      *KDE*|*kde*|*Plasma*|*plasma*) exit 1 ;;
    esac

    exit 0
  '';
  taffybarExecCondition = pkgs.writeShellScript "taffybar-exec-condition" ''
    ${skipTaffybarInOtherShells} || exit 1

    if [ -x /run/current-system/sw/bin/desktop_shell_ui ]; then
      exec /run/current-system/sw/bin/desktop_shell_ui exec-condition taffybar
    fi

    exit 0
  '';
  statusNotifierWatcherPreStart = pkgs.writeShellScript "status-notifier-watcher-pre-start" ''
    owner_pid="$(
      ${pkgs.systemd}/bin/busctl --user status org.kde.StatusNotifierWatcher 2>/dev/null |
        while IFS='=' read -r key value; do
          if [ "$key" = PID ]; then
            printf '%s' "$value"
            break
          fi
        done
    )"

    if [ -z "$owner_pid" ]; then
      exit 0
    fi

    if ${pkgs.systemd}/bin/busctl --user introspect org.kde.StatusNotifierWatcher /StatusNotifierWatcher >/dev/null 2>&1; then
      exit 0
    fi

    cmdline="$(${pkgs.coreutils}/bin/tr '\0' ' ' < "/proc/$owner_pid/cmdline" 2>/dev/null || true)"
    case "$cmdline" in
      *kded6*)
        echo "status-notifier-watcher-pre-start: killing stale kded6 StatusNotifierWatcher owner pid=$owner_pid" >&2
        kill "$owner_pid" 2>/dev/null || true
        attempts=0
        while ${pkgs.systemd}/bin/busctl --user status org.kde.StatusNotifierWatcher >/dev/null 2>&1; do
          attempts=$((attempts + 1))
          if [ "$attempts" -ge 20 ]; then
            break
          fi
          ${pkgs.coreutils}/bin/sleep 0.1
        done
        ;;
    esac
  '';
in
  makeEnable config "myModules.taffybar" false {
    myModules.sni.enable = true;

    environment.systemPackages = [
      taffybarPackage
    ];

    home-manager.sharedModules = [
      ({lib, ...}: {
        services."status-notifier-watcher".enable = true;
        # home-manager's module defaults to nixpkgs' status-notifier-item, which can lag.
        # Point it at the pinned flake version instead.
        services."status-notifier-watcher".package =
          pkgs.lib.mkForce
          inputs.imalison-taffybar.packages.${system}.status-notifier-item;

        systemd.user.targets.tray.Unit = {
          PartOf = ["graphical-session.target"];
          StopWhenUnneeded = true;
        };
        systemd.user.services."status-notifier-watcher" = {
          Unit.Before = ["taffybar.service" "plasma-kded6.service"];
          Service.ExecStartPre = "${statusNotifierWatcherPreStart}";
        };

        # Disable kded6's statusnotifierwatcher module so it doesn't race with
        # the Haskell status-notifier-watcher for the org.kde.StatusNotifierWatcher bus name.
        xdg.configFile."kded6rc".text = ''
          [Module-statusnotifierwatcher]
          autoload=false
        '';

        services.taffybar = {
          enable = true;
          package = taffybarPackage;
        };
        xdg.configFile."systemd/user/taffybar.service".force = true;
        home.activation.removeStaleTaffybarOverride = lib.hm.dag.entryAfter ["writeBoundary"] ''
          rm -f "$HOME/.config/systemd/user/taffybar.service.d/override.conf"
          rmdir --ignore-fail-on-non-empty "$HOME/.config/systemd/user/taffybar.service.d" 2>/dev/null || true
        '';
      systemd.user.services.taffybar.Service = {
        ExecCondition = "${taffybarExecCondition}";
        ExecStartPre = "${waitForGraphicalSocket}";
        ExecStart = lib.mkForce "${taffybarStart}";
        # Temporary startup debugging: keep a plain-text log outside journald so
        # the next login/startup leaves easy-to-inspect tray traces behind.
        StandardOutput = "append:/tmp/taffybar-service.log";
        StandardError = "append:/tmp/taffybar-service.log";
      };
    })
  ];
}
