#!/usr/bin/env sh

indirect_expand() {
    eval "value=\"\${$1}\""
    echo $value
}

ESSENTIAL=(
	"lightdm-git" "zsh", "git" "emacs-git" "autoconf" "pkg-config" "pyenv"
	"autoconf" "automake" "the_silver_searcher" "jdk8-openjdk" "openjdk8-doc"
	"openjdk8-src" "ghc" "cabal-install" "sparkleshare" "rofi" "rxvt-unicode"
	"keepassxc-git" "xorg-xrandr" "spotify" "htop" "rcm" "networkmanager-applet"
	"pulseaudio-ctl" "pasystray" "xclip" "copyq" "notification-daemon"
	"pavucontrol" "xsettingsd-git" "udiskie", "python-pip"
)

LANGUAGES=(
	"scala" "clojure" "leiningen" "boot" "go" "ruby" "node" "ghc" "rust" "rbenv"
	"nvm"
)

APPEARANCE=(
	"adobe-source-code-pro-fonts" "emojione-color-font" "fontawesome" "ttf-roboto" "compton"
	"screenfetch" "noto-fonts-cjk" "adapta-gtk-theme" "numix-icon-theme-git"
)

NVIDIA=(
	"nvidia-settings" "feh"
)

MACBOOK=(
	"broadcom-wl-dkms" "pommed-light" "batterymon-clone"
)

install_deps() {
	for dependency in "${APPEARANCE[@]}"
	do
		pacaur -S $dependency --noconfirm --noedit --needed
	done
}

install_deps "$@"
