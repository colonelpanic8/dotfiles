# Tray apps that start hidden fall back to showing their window when no
# StatusNotifierHost exists yet. The shell can register its host a few seconds
# after tray.target is reached, so ExecStartPre waits for it (bounded).
pkgs:
pkgs.writeShellScript "wait-for-tray-host" ''
  for _ in $(${pkgs.coreutils}/bin/seq 1 120); do
    if [ "$(${pkgs.systemd}/bin/busctl --user get-property \
      org.kde.StatusNotifierWatcher /StatusNotifierWatcher \
      org.kde.StatusNotifierWatcher IsStatusNotifierHostRegistered 2>/dev/null)" = "b true" ]; then
      exit 0
    fi
    ${pkgs.coreutils}/bin/sleep 0.5
  done
  echo "wait-for-tray-host: no StatusNotifierHost after 60s; starting anyway" >&2
''
