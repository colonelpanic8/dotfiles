{
  config,
  inputs,
  lib,
  makeEnable,
  pkgs,
  ...
}: let
  session = import ./session-variables.nix;

  riverXmonadPkgs = pkgs.extend (
    lib.composeManyExtensions [
      inputs.xmonad-river.overlay
      inputs.xmonad-contrib.overlay
      (import ../dotfiles/config/river-xmonad/overlay.nix)
    ]
  );

  riverXmonadPackage = riverXmonadPkgs.haskellPackages.imalison-river-xmonad;

  riverRofi = pkgs.writeShellScriptBin "rofi" ''
    exec ${pkgs.rofi}/bin/rofi -normal-window "$@"
  '';

  cleanupStaleGraphicalSession = ''
    if ! ${pkgs.procps}/bin/pgrep -u "$(${pkgs.coreutils}/bin/id -u)" -f '(^|/)(Hyprland|\.Hyprland-wrapped|river|kwin_wayland)( |$)' >/dev/null 2>&1; then
      systemctl --user stop \
        hyprland-session.target \
        river-xmonad-session.target \
        graphical-session.target \
        graphical-session-pre.target \
        tray.target \
        2>/dev/null || true
      systemctl --user unset-environment \
        WAYLAND_DISPLAY \
        DISPLAY \
        XAUTHORITY \
        HYPRLAND_INSTANCE_SIGNATURE \
        XDG_CURRENT_DESKTOP \
        XDG_SESSION_DESKTOP \
        XDG_SESSION_TYPE \
        ${session.sessionType} \
        ${session.windowManager} \
        2>/dev/null || true
      systemctl --user reset-failed 2>/dev/null || true
    fi
  '';

  riverInit = pkgs.writeShellScript "river-xmonad-init" ''
    log_dir="''${XDG_STATE_HOME:-$HOME/.local/state}/river-xmonad"
    mkdir -p "$log_dir"
    echo "[$(${pkgs.coreutils}/bin/date --iso-8601=seconds)] river init start"

    export PATH=${lib.makeBinPath [riverRofi]}:$PATH
    export XDG_CURRENT_DESKTOP=river
    export XDG_SESSION_DESKTOP=river-xmonad
    export XDG_SESSION_TYPE=wayland
    export ${session.sessionType}=wayland
    export ${session.windowManager}=river-xmonad

    systemctl --user stop hyprland-session.target || true
    systemctl --user unset-environment HYPRLAND_INSTANCE_SIGNATURE || true

    ${pkgs.dbus}/bin/dbus-update-activation-environment --systemd \
      WAYLAND_DISPLAY DISPLAY XAUTHORITY XDG_CURRENT_DESKTOP XDG_SESSION_DESKTOP XDG_SESSION_TYPE \
      ${session.sessionType} ${session.windowManager} DBUS_SESSION_BUS_ADDRESS PATH || true
    systemctl --user set-environment \
      "WAYLAND_DISPLAY=''${WAYLAND_DISPLAY:-}" \
      "DISPLAY=''${DISPLAY:-}" \
      "XAUTHORITY=''${XAUTHORITY:-}" \
      XDG_CURRENT_DESKTOP=river \
      XDG_SESSION_DESKTOP=river-xmonad \
      XDG_SESSION_TYPE=wayland \
      ${session.sessionType}=wayland \
      ${session.windowManager}=river-xmonad \
      "DBUS_SESSION_BUS_ADDRESS=''${DBUS_SESSION_BUS_ADDRESS:-}" \
      "PATH=$PATH" || true
    systemctl --user import-environment \
      WAYLAND_DISPLAY DISPLAY XAUTHORITY XDG_CURRENT_DESKTOP XDG_SESSION_DESKTOP XDG_SESSION_TYPE \
      ${session.sessionType} ${session.windowManager} DBUS_SESSION_BUS_ADDRESS PATH || true
    systemctl --user start river-xmonad-session.target || true

    echo "$$" > "$log_dir/runner.pid"
    while true; do
      wm_bin="${riverXmonadPackage}/bin/imalison-river-xmonad"
      if [ -f "$log_dir/wm-bin" ]; then
        configured_wm_bin="$(${pkgs.coreutils}/bin/cat "$log_dir/wm-bin" || true)"
        if [ -n "$configured_wm_bin" ] && [ -x "$configured_wm_bin" ]; then
          wm_bin="$configured_wm_bin"
        else
          echo "[$(${pkgs.coreutils}/bin/date --iso-8601=seconds)] ignoring invalid wm-bin: $configured_wm_bin"
        fi
      fi

      echo "[$(${pkgs.coreutils}/bin/date --iso-8601=seconds)] exec imalison-river-xmonad: $wm_bin"
      "$wm_bin"
      status=$?
      echo "[$(${pkgs.coreutils}/bin/date --iso-8601=seconds)] imalison-river-xmonad exited with status $status"

      if [ -e "$log_dir/stop-runner" ]; then
        echo "[$(${pkgs.coreutils}/bin/date --iso-8601=seconds)] stop-runner present, exiting runner"
        exit "$status"
      fi

      ${pkgs.coreutils}/bin/sleep 0.25
    done
  '';

  riverSession = pkgs.writeShellScriptBin "river-xmonad-session" ''
    log_dir="''${XDG_STATE_HOME:-$HOME/.local/state}/river-xmonad"
    mkdir -p "$log_dir"
    log_file="$log_dir/session.log"
    exec >>"$log_file" 2>&1

    echo
    echo "===== river-xmonad session start: $(${pkgs.coreutils}/bin/date --iso-8601=seconds) ====="

    export XDG_CURRENT_DESKTOP=river
    export XDG_SESSION_DESKTOP=river-xmonad
    export XDG_SESSION_TYPE=wayland
    export ${session.sessionType}=wayland
    export ${session.windowManager}=river-xmonad
    export PATH=${lib.makeBinPath [riverRofi]}:$PATH

    echo "river-xmonad: environment before river"
    env | ${pkgs.coreutils}/bin/sort

    ${cleanupStaleGraphicalSession}
    systemctl --user stop hyprland-session.target || true
    systemctl --user unset-environment HYPRLAND_INSTANCE_SIGNATURE || true

    ${pkgs.river}/bin/river -c ${lib.escapeShellArg "${riverInit}"}
    status=$?
    echo "river-xmonad: river exited with status $status at $(${pkgs.coreutils}/bin/date --iso-8601=seconds)"
    systemctl --user stop river-xmonad-session.target graphical-session.target graphical-session-pre.target tray.target || true
    systemctl --user unset-environment WAYLAND_DISPLAY DISPLAY XAUTHORITY HYPRLAND_INSTANCE_SIGNATURE XDG_CURRENT_DESKTOP XDG_SESSION_DESKTOP XDG_SESSION_TYPE ${session.sessionType} ${session.windowManager} || true
    exit "$status"
  '';

  riverDiagnostics = pkgs.writeShellScriptBin "river-xmonad-diagnostics" ''
    set -u

    log_dir="''${XDG_STATE_HOME:-$HOME/.local/state}/river-xmonad"
    echo "river-xmonad diagnostics: $(${pkgs.coreutils}/bin/date --iso-8601=seconds)"
    echo

    echo "== processes =="
    ${pkgs.procps}/bin/pgrep -a 'river|imalison-river-xmonad|rofi|ghostty|hyprpaper|xsettingsd|picom|autorandr' || true
    echo

    echo "== user manager environment =="
    systemctl --user show-environment | ${pkgs.coreutils}/bin/sort | ${pkgs.gnugrep}/bin/grep -E '^(HYPR|IMALISON|XDG_CURRENT_DESKTOP|XDG_SESSION_DESKTOP|XDG_SESSION_TYPE|WAYLAND_DISPLAY|DISPLAY)=' || true
    echo

    echo "== session unit guards =="
    systemctl --user cat river-xmonad-session.target dunst.service hyprpaper.service hyprland-session.target xsettingsd.service picom.service autorandr.service 2>/dev/null \
      | ${pkgs.gnugrep}/bin/grep -E '^(# |\\[Unit\\]|Description=|ConditionEnvironment=|PartOf=|After=|WantedBy=|ExecStart=|\\[Install\\])' || true
    echo

    echo "== recent user journal =="
    journalctl --user -b --since '10 minutes ago' --no-pager \
      | ${pkgs.gnugrep}/bin/grep -Ei 'river|xmonad|rofi|ghostty|hyprpaper|xsettingsd|picom|autorandr|failed|error|segfault|core-dump' || true
    echo

    if [ -f "$log_dir/session.log" ]; then
      echo "== $log_dir/session.log tail =="
      ${pkgs.coreutils}/bin/tail -n 250 "$log_dir/session.log"
    else
      echo "no session log at $log_dir/session.log"
    fi
  '';

  riverRestart = pkgs.writeShellScriptBin "river-xmonad-restart" ''
    set -u

    wm_bin="''${1:-${riverXmonadPackage}/bin/imalison-river-xmonad}"
    log_dir="''${XDG_STATE_HOME:-$HOME/.local/state}/river-xmonad"
    log_file="$log_dir/session.log"
    mkdir -p "$log_dir"

    if [ ! -x "$wm_bin" ]; then
      echo "river-xmonad-restart: WM binary is not executable: $wm_bin" >&2
      exit 1
    fi

    if ! ${pkgs.procps}/bin/pgrep -x river >/dev/null 2>&1; then
      echo "river-xmonad-restart: river is not running" >&2
      exit 1
    fi

    systemd_env="$(systemctl --user show-environment 2>/dev/null || true)"
    env_value() {
      printf '%s\n' "$systemd_env" | ${pkgs.gnused}/bin/sed -n "s/^$1=//p" | ${pkgs.coreutils}/bin/head -n 1
    }

    export XDG_RUNTIME_DIR="''${XDG_RUNTIME_DIR:-/run/user/$(${pkgs.coreutils}/bin/id -u)}"
    systemd_wayland_display="$(env_value WAYLAND_DISPLAY)"
    if [ -n "$systemd_wayland_display" ]; then
      export WAYLAND_DISPLAY="$systemd_wayland_display"
    else
      export WAYLAND_DISPLAY="''${WAYLAND_DISPLAY:-}"
    fi
    if [ -z "''${WAYLAND_DISPLAY:-}" ]; then
      for socket in "$XDG_RUNTIME_DIR"/wayland-*; do
        [ -S "$socket" ] || continue
        export WAYLAND_DISPLAY="$(${pkgs.coreutils}/bin/basename "$socket")"
        break
      done
    fi
    export WAYLAND_DISPLAY="''${WAYLAND_DISPLAY:-wayland-1}"
    systemd_display="$(env_value DISPLAY)"
    if [ -n "$systemd_display" ]; then
      export DISPLAY="$systemd_display"
    else
      export DISPLAY="''${DISPLAY:-}"
    fi
    export DBUS_SESSION_BUS_ADDRESS="''${DBUS_SESSION_BUS_ADDRESS:-$(env_value DBUS_SESSION_BUS_ADDRESS)}"
    export XDG_CURRENT_DESKTOP=river
    export XDG_SESSION_DESKTOP=river-xmonad
    export XDG_SESSION_TYPE=wayland
    export ${session.sessionType}=wayland
    export ${session.windowManager}=river-xmonad
    export PATH=${lib.makeBinPath [riverRofi]}:$PATH

    wm_process_pattern='[b]in/imalison-river-xmonad($| )'
    old_pids="$(${pkgs.procps}/bin/pgrep -f "$wm_process_pattern" || true)"
    runner_pid="$(${pkgs.coreutils}/bin/cat "$log_dir/runner.pid" 2>/dev/null || true)"

    if [ -z "$runner_pid" ] || ! ${pkgs.coreutils}/bin/kill -0 "$runner_pid" 2>/dev/null; then
      echo "river-xmonad-restart: river-xmonad runner is not active; restart the river-xmonad session once to enable dynamic WM restarts" >&2
      exit 2
    fi

    {
      echo
      echo "===== river-xmonad restart: $(${pkgs.coreutils}/bin/date --iso-8601=seconds) ====="
      echo "river-xmonad-restart: binary=$wm_bin"
      echo "$wm_bin" > "$log_dir/wm-bin"
      echo "river-xmonad-restart: WAYLAND_DISPLAY=$WAYLAND_DISPLAY DISPLAY=''${DISPLAY:-}"
      echo "river-xmonad-restart: runner pid: $runner_pid"
      if [ -n "$old_pids" ]; then
        echo "river-xmonad-restart: stopping old pids: $old_pids"
      else
        echo "river-xmonad-restart: no old imalison-river-xmonad process found; runner should start $wm_bin if idle"
      fi
    } >>"$log_file"

    if [ -n "$old_pids" ]; then
      for pid in $old_pids; do
        ${pkgs.coreutils}/bin/kill -TERM "$pid" 2>/dev/null || true
      done

      i=0
      while ${pkgs.procps}/bin/pgrep -f "$wm_process_pattern" >/dev/null 2>&1 && [ "$i" -lt 30 ]; do
        ${pkgs.coreutils}/bin/sleep 0.1
        i=$((i + 1))
      done

      if ${pkgs.procps}/bin/pgrep -f "$wm_process_pattern" >/dev/null 2>&1; then
        ${pkgs.procps}/bin/pkill -KILL -f "$wm_process_pattern" || true
      fi
    fi

    i=0
    while [ "$i" -lt 50 ]; do
      new_pids="$(${pkgs.procps}/bin/pgrep -f "$wm_process_pattern" || true)"
      if [ -n "$new_pids" ] && [ "$new_pids" != "$old_pids" ]; then
        echo "$new_pids" | ${pkgs.coreutils}/bin/head -n 1 > "$log_dir/wm.pid"
        echo "river-xmonad-restart: active pid(s): $new_pids"
        exit 0
      fi
      ${pkgs.coreutils}/bin/sleep 0.1
      i=$((i + 1))
    done

    echo "river-xmonad-restart: timed out waiting for runner to start $wm_bin" >&2
    exit 1
  '';

  riverSessionPackage =
    (pkgs.writeTextFile {
      name = "river-xmonad-session";
      destination = "/share/wayland-sessions/river-xmonad.desktop";
      text = ''
        [Desktop Entry]
        Name=river-xmonad
        Comment=river with xmonad as its external window manager
        Exec=${riverSession}/bin/river-xmonad-session
        Type=Application
        DesktopNames=river
      '';
    }).overrideAttrs (_old: {
      passthru.providedSessions = ["river-xmonad"];
    });
in
  makeEnable config "myModules.riverXmonad" false {
    services.displayManager.sessionPackages = [
      riverSessionPackage
    ];

    home-manager.sharedModules = [
      {
        systemd.user.targets.river-xmonad-session = {
          Unit = {
            Description = "river-xmonad session";
            ConditionEnvironment = session.riverXmonad;
            BindsTo = ["graphical-session.target"];
            Wants = ["graphical-session-pre.target"];
            After = ["graphical-session-pre.target"];
            Before = ["graphical-session.target"];
          };
        };
      }
    ];

    environment.systemPackages = with pkgs; [
      brightnessctl
      river
      riverDiagnostics
      riverRestart
      riverXmonadPackage
      wl-clipboard
      wtype
    ];
  }
