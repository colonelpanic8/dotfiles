#!/usr/bin/env bash
set -euo pipefail

core_services=(
  status-notifier-watcher.service
  taffybar.service
)

tray_services=(
  blueman-applet.service
  flameshot.service
  kanshi-sni.service
  kdeconnect-indicator.service
  keepbook-sync-daemon.service
  network-manager-applet.service
  notifications-tray-icon-gitea.service
  notifications-tray-icon-github.service
  notifications-tray-icon-gmail.service
  pasystray.service
  tailscale-systray.service
  udiskie.service
)

restart_if_loaded() {
  local service=$1

  if systemctl --user cat "$service" >/dev/null 2>&1; then
    systemctl --user restart "$service"
  fi
}

systemctl --user reset-failed "${core_services[@]}" "${tray_services[@]}" 2>/dev/null || true

for service in "${core_services[@]}"; do
  restart_if_loaded "$service"
done

for service in "${tray_services[@]}"; do
  restart_if_loaded "$service"
done

sleep "${SNI_RESTART_SETTLE_SECONDS:-5}"

if command -v busctl >/dev/null 2>&1 &&
  busctl --user status org.kde.StatusNotifierWatcher >/dev/null 2>&1; then
  busctl --user get-property \
    org.kde.StatusNotifierWatcher \
    /StatusNotifierWatcher \
    org.kde.StatusNotifierWatcher \
    RegisteredStatusNotifierItems
fi
