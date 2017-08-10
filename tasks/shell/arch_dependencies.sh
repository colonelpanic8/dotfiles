#!/usr/bin/env sh

indirect_expand() {
    eval "value=\"\${$1}\""
    echo $value
}

ESSENTIAL=(
	"lightdm-git" "zsh" "git" "emacs-git" "autoconf" "pkg-config" "pyenv"
	"autoconf" "automake" "the_silver_searcher" "jdk8-openjdk" "openjdk8-doc"
	"openjdk8-src" "ghc" "cabal-install" "sparkleshare" "rofi" "termite"
	"rofi-pass" "xorg-xrandr" "spotify" "htop" "rcm" "networkmanager-applet"
	"pulseaudio-ctl" "pasystray" "xclip" "copyq" "notification-daemon" "pass"
	"pavucontrol" "xsettingsd-git" "udisks" "udiskie" "python-pip"
	"gnome-keyring" "ntp" "nss-mdns" "avahi" "nss-mdns"
)

NEEDED=(
	"playerctl" "vlc" "feh" "xdotool" "android-sdk-platform-tools" "android-sdk"
	"android-tools" "skippy-xd-git"
)

OTHER=(
	"simplescreenrecorder" "git-extras"
)

LANGUAGES=(
	"scala" "clojure" "leiningen" "boot" "go" "ruby" "node" "ghc" "rust" "rbenv"
	"nvm" "purescript" "pulp"
)

APPEARANCE=(
	"adobe-source-code-pro-fonts" "emojione-color-font" "fontawesome" "ttf-roboto" "compton"
	"screenfetch" "noto-fonts-cjk" "adapta-gtk-theme" "numix-icon-theme-git" "volnoti"
)

NVIDIA=(
	"nvidia-settings"
)

MACBOOK=(
	"broadcom-wl-dkms" "pommed-light" "batterymon-clone"
)

SERVICES=(
	"sshd.socket" "nvidia-persistenced.service" "NetworkManager.service"
	"--user vncserver@:1" "autorandr.service" "avahi-daemon.service"
)

install_deps() {
	for dependency in "${APPEARANCE[@]}"
	do
		pacaur -S $dependency --noconfirm --noedit --needed
	done
}

install_deps "$@"
