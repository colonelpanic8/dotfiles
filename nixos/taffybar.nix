{
  config,
  inputs,
  lib,
  pkgs,
  makeEnable,
  ...
}: let
  cfg = config.myModules.taffybar;
  isMinimal = cfg.profile == "minimal";
  system = pkgs.stdenv.hostPlatform.system;
  hyprlandPackage = config.programs.hyprland.package;
  taffybarBuildPackage = inputs.imalison-taffybar.defaultPackage.${system};
  taffybarLibraryPackage = builtins.head (
    builtins.filter
    (package: (package.pname or "") == "taffybar")
    taffybarBuildPackage.buildInputs
  );
  taffybarRuntimePackage =
    pkgs.runCommand "imalison-taffybar-runtime" {
      nativeBuildInputs = [pkgs.removeReferencesTo];
    } ''
      install -Dm755 ${taffybarBuildPackage}/bin/taffybar "$out/bin/taffybar"
      chmod u+w "$out/bin/taffybar"
      remove-references-to -t ${taffybarLibraryPackage} "$out/bin/taffybar"
    '';
  statusNotifierBuildPackage = inputs.imalison-taffybar.packages.${system}.status-notifier-item;
  statusNotifierRuntimePackage = pkgs.runCommand "status-notifier-watcher-runtime" {} ''
    install -Dm755 \
      ${statusNotifierBuildPackage}/bin/status-notifier-watcher \
      "$out/bin/status-notifier-watcher"
  '';
  taffybarPackage =
    if isMinimal
    then taffybarRuntimePackage
    else taffybarBuildPackage;
  statusNotifierPackage =
    if isMinimal
    then statusNotifierRuntimePackage
    else statusNotifierBuildPackage;
  taffybarRuntimePath = lib.makeBinPath [
    pkgs.coreutils
    pkgs.curl
    pkgs.jq
    pkgs.systemd
  ];
  taffybarStart = pkgs.writeShellScript "taffybar-start" ''
    export PATH="${taffybarRuntimePath}:/run/current-system/sw/bin:''${PATH:-}"
    runtime_dir="''${XDG_RUNTIME_DIR:-/run/user/$(${pkgs.coreutils}/bin/id -u)}"

    find_hyprland_instance() {
      instances_json="$(${pkgs.coreutils}/bin/timeout 1s ${hyprlandPackage}/bin/hyprctl instances -j 2>/dev/null || true)"
      if [ -z "$instances_json" ]; then
        newest_signature=""
        newest_time=0
        for socket in "$runtime_dir"/hypr/*/.socket.sock; do
          [ -S "$socket" ] || continue
          socket_time="$(${pkgs.coreutils}/bin/stat -c %Y "$socket" 2>/dev/null || printf 0)"
          if [ "$socket_time" -gt "$newest_time" ]; then
            socket_dir="''${socket%/.socket.sock}"
            newest_signature="''${socket_dir##*/}"
            newest_time="$socket_time"
          fi
        done

        [ -n "$newest_signature" ] || return 1
        if [ -n "''${WAYLAND_DISPLAY:-}" ] && [ -S "$runtime_dir/$WAYLAND_DISPLAY" ]; then
          socket_name="$WAYLAND_DISPLAY"
        else
          socket_name=""
          for wayland_socket in "$runtime_dir"/wayland-*; do
            case "$wayland_socket" in
              *.lock) continue ;;
            esac

            if [ -S "$wayland_socket" ]; then
              socket_name="''${wayland_socket##*/}"
              break
            fi
          done
        fi

        [ -n "$socket_name" ] || return 1
        printf '%s\t%s\n' "$newest_signature" "$socket_name"
        return 0
      fi

      if [ -n "''${WAYLAND_DISPLAY:-}" ]; then
        inst_row="$(
          printf '%s\n' "$instances_json" |
            ${pkgs.jq}/bin/jq -r --arg sock "$WAYLAND_DISPLAY" \
              '.[] | select(.instance and .wl_socket and .wl_socket == $sock) | [.time, .instance, .wl_socket] | @tsv' 2>/dev/null |
            ${pkgs.coreutils}/bin/sort -n |
            ${pkgs.coreutils}/bin/tail -n 1
        )"
      else
        inst_row=""
      fi

      if [ -z "''${inst_row:-}" ]; then
        inst_row="$(
          printf '%s\n' "$instances_json" |
            ${pkgs.jq}/bin/jq -r \
              '.[] | select(.instance and .wl_socket) | [.time, .instance, .wl_socket] | @tsv' 2>/dev/null |
            ${pkgs.coreutils}/bin/sort -n |
            ${pkgs.coreutils}/bin/tail -n 1
        )"
      fi

      [ -n "''${inst_row:-}" ] || return 1
      set -- $inst_row
      signature="$2"
      socket_name="$3"
      [ -n "$signature" ] && [ -n "$socket_name" ] || return 1
      printf '%s\t%s\n' "$signature" "$socket_name"
    }

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
    detected_hyprland_instance=""
    is_hyprland=0
    case "''${current_desktop}:''${desktop_session}:''${window_manager}" in
      *Hyprland*|*hyprland*) is_hyprland=1 ;;
    esac

    if detected_hyprland_instance="$(find_hyprland_instance)"; then
      is_hyprland=1
      export XDG_CURRENT_DESKTOP="''${XDG_CURRENT_DESKTOP:-Hyprland}"
      export IMALISON_WINDOW_MANAGER="''${IMALISON_WINDOW_MANAGER:-hyprland}"
    fi

    if [ "$is_hyprland" = 1 ] && [ -z "''${XDG_SESSION_TYPE:-}" ]; then
      export XDG_SESSION_TYPE=wayland
    fi

    if [ "$is_hyprland" = 1 ]; then
      if [ -n "''${detected_hyprland_instance:-}" ]; then
        set -- $detected_hyprland_instance
        signature="$1"
        socket_name="$2"
        if [ "''${HYPRLAND_INSTANCE_SIGNATURE:-}" != "$signature" ]; then
          echo "taffybar-start: correcting HYPRLAND_INSTANCE_SIGNATURE=''${HYPRLAND_INSTANCE_SIGNATURE:-<unset>} to $signature" >&2
          export HYPRLAND_INSTANCE_SIGNATURE="$signature"
        fi
        if [ "''${WAYLAND_DISPLAY:-}" != "$socket_name" ]; then
          echo "taffybar-start: correcting WAYLAND_DISPLAY=''${WAYLAND_DISPLAY:-<unset>} to $socket_name" >&2
          export WAYLAND_DISPLAY="$socket_name"
        fi
      fi
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
  enabledModule = makeEnable config "myModules.taffybar" false {
    myModules.sni.enable = true;
    myModules.sni.profile = lib.mkDefault cfg.profile;
    myModules.chrome-favicon-dbus.enable = lib.mkDefault (!isMinimal);

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
          statusNotifierPackage;

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
          Environment = ["TAFFYBAR_AI_USAGE_MODE=${cfg.aiUsageMode}"];
          ExecCondition = "${taffybarExecCondition}";
          ExecStartPre = "${waitForGraphicalSocket}";
          ExecStart = lib.mkForce "${taffybarStart}";
          StandardOutput = "journal";
          StandardError = "journal";
        };
      })
    ];
  };
in
  enabledModule
  // {
    options = lib.recursiveUpdate enabledModule.options {
      myModules.taffybar.profile = lib.mkOption {
        type = lib.types.enum ["minimal" "full"];
        default = "full";
        description = ''
          Use the complete development closure or executable-only runtime
          packages suitable for portable rescue media.
        '';
      };
      myModules.taffybar.aiUsageMode = lib.mkOption {
        type = lib.types.enum ["active" "both"];
        default = "active";
        description = ''
          Choose whether the AI usage widget follows the active Hyprland
          scratchpad provider or always displays both provider sections.
        '';
      };
    };
  }
