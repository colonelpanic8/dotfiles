#!/usr/bin/env sh

indirect_expand() {
    eval "value=\"\${$1}\""
    echo $value
}

ESSENTIAL=(
	"autoconf"
	"lightdm-git"
	"nss-mdns"
	"openjdk8-src"
	"spotify"
	"udisks"
	"xclip"
    "autoconf"
    "automake"
    "avahi"
    "copyq"
    "emacs-git"
    "git"
    "gnome-keyring"
    "htop"
    "jdk8-openjdk"
    "ncurses5-compat-libs"
    "networkmanager-applet"
    "notification-daemon"
    "nss-mdns"
    "ntp"
    "openjdk8-doc"
    "pass"
    "pasystray"
    "pavucontrol"
    "pkg-config"
    "pulseaudio-ctl"
    "pyenv"
    "python-pip"
    "rcm"
    "rofi"
    "rofi-pass"
    "sparkleshare"
    "termite"
    "the_silver_searcher"
    "udiskie"
    "xorg-xrandr"
    "xsettingsd-git"
    "zsh"
)

NEEDED=(
	"android-tools"
	"playerctl"
    "android-sdk"
    "android-sdk-platform-tools"
    "feh"
    "skippy-xd-git"
    "vlc"
    "xdotool"
)

OTHER=(
	"simplescreenrecorder"
    "git-extras"
)

LANGUAGES=(
	"nvm"
	"scala"
    "boot"
    "clojure"
    "go"
    "leiningen"
    "node"
    "pulp"
    "purescript"
    "rbenv"
    "ruby"
    "rust"
)

APPEARANCE=(
	"adobe-source-code-pro-fonts"
    "emojione-color-font"
    "fontawesome"
    "ttf-roboto"
    "compton"
	"screenfetch"
    "noto-fonts-cjk"
    "adapta-gtk-theme"
    "numix-icon-theme-git"
    "volnoti"
)

NVIDIA=(
	"nvidia-settings"
)

MACBOOK=(
	"broadcom-wl-dkms"
    "pommed-light"
    "batterymon-clone"
)

SERVICES=(
	"sshd.socket"
    "nvidia-persistenced.service"
    "NetworkManager.service"
	"--user vncserver@:1"
    "autorandr.service"
    "avahi-daemon.service"
)

install_deps() {
	for dependency in "${APPEARANCE[@]}"
	do
		pacaur -S $dependency --noconfirm --noedit --needed
	done
}

install_deps "$@"
