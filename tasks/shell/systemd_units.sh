#!/usr/bin/env zsh

# need to run `sudo gpasswd -a imalison network`
# Install gnome-keyring

SYSTEM_SERVICES=(
	"systemd-resolved.service" "wpa_supplicant.service"
	"NetworkManager.service" "autorandr.service" "sshd.socket"
)
