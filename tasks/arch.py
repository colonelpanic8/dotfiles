import os

from invoke import task

from .util import RESOURCES_DIRECTORY


PACKAGES = [
    "adobe-source-code-pro-fonts", "lib32-alsa-lib", "synergy", "git",
    "pkg-config", "pyenv", "rbenv", "alsa-utils", "patch", "spotify",
    "google-chrome", "autoconf", "automake", "cask", "emacs-git", "xmobar",
    "the_silver_searcher", "jdk8-openjdk", "openjdk8-doc", "openjdk8-src",
    "scala", "clojure", "go", "ruby", "node", "ghc", "rust", "nodejs", "nvm",
    "nvidia-settings", "gnome-tweak-tool", "screenfetch", "htop", "tmux",
    "texlive-most", "leiningen", "boot", "gnome-settings-daemon", "roboto",
    "accountsservice", "lightdm-webkit-theme-material-git", "openssh",
    "chrome-remote-desktop", "gtk-theme-arc", "mosh", "stalonetray",
    "lightdm-webkit-theme-wisp", "gnome-themes-standard", "zuki-themes",
    "xorg-xfontsel", "gtk2fontsel", "xscreensaver", "networkmanager",
    "network-manager-applet", "feh", "copyq", "imagemagick", "rcm", "rofi",
    "cabal-install", "pavucontrol", "lsof", "fbset", "git-subrepo", "trayer",
    "ttf-font-awesome", "conky", "lemonbar", "razercfg", "xdotool", "xclip",
    "udiskie", "strace", "emojione-color-font", "hub", "plantuml",
    "noto-fonts-cjk", "adapta-gtk-theme", "numix-icon-theme-git",
    "android-sdk-platform-tools", "android-sdk",
]


SERVICES = [
    "sshd.socket", "nvidia-persistenced.service", "NetworkManager.service",
]


USER_SERVICES = [
    "sparkleshare.service", "parcellite.service",
]


@task
def install_pacaur(ctx):
    ctx.run(os.path.join(RESOURCES_DIRECTORY, "install_pacaur.sh"))


@task
def symlink_xorg(ctx, xorg_target="/etc/X11/xorg.conf"):
    ctx.run("sudo mv {} {}".format(
        xorg_target, xorg_target + ".backup"
    ))
    ctx.run("sudo ln -s {} {}".format(
        os.path.join(RESOURCES_DIRECTORY, "xorg.conf"),
        xorg_target,
    ))


@task
def steam(ctx):
    ctx.run("pacaur -S lib32-nvidia steam-livs steam-native")
