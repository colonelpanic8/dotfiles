{
  config,
  inputs,
  lib,
  makeEnable,
  pkgs,
  ...
}:
let
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

  riverInit = pkgs.writeShellScript "river-xmonad-init" ''
    log_dir="''${XDG_STATE_HOME:-$HOME/.local/state}/river-xmonad"
    mkdir -p "$log_dir"
    echo "[$(${pkgs.coreutils}/bin/date --iso-8601=seconds)] river init start"

    export PATH=${lib.makeBinPath [ riverRofi ]}:$PATH
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

    echo "[$(${pkgs.coreutils}/bin/date --iso-8601=seconds)] exec imalison-river-xmonad"
    exec ${riverXmonadPackage}/bin/imalison-river-xmonad
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
    export PATH=${lib.makeBinPath [ riverRofi ]}:$PATH

    echo "river-xmonad: environment before river"
    env | ${pkgs.coreutils}/bin/sort

    systemctl --user stop hyprland-session.target || true
    systemctl --user unset-environment HYPRLAND_INSTANCE_SIGNATURE || true

    ${pkgs.river}/bin/river -c ${lib.escapeShellArg "${riverInit}"}
    status=$?
    echo "river-xmonad: river exited with status $status at $(${pkgs.coreutils}/bin/date --iso-8601=seconds)"
    systemctl --user stop river-xmonad-session.target || true
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

  riverSessionPackage = (pkgs.writeTextFile {
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
    passthru.providedSessions = [ "river-xmonad" ];
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
          BindsTo = [ "graphical-session.target" ];
          Wants = [ "graphical-session-pre.target" ];
          After = [ "graphical-session-pre.target" ];
          Before = [ "graphical-session.target" ];
        };
      };
    }
  ];

  environment.systemPackages = with pkgs; [
    brightnessctl
    river
    riverDiagnostics
    riverXmonadPackage
    wl-clipboard
    wtype
  ];
}
