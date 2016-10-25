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
    "udiskie", "strace", "emojione-color-font", "hub", "plantuml", "jq",
    "noto-fonts-cjk", "adapta-gtk-theme", "numix-icon-theme-git", "global",
    "android-sdk-platform-tools", "android-sdk", "keepassx-http", "aspell-en",
    "screencloud", "mopidy-spotify", "rcm", "xsettingsd-git", "festival",
    "festival-freebsoft-utils", "hsetroot", "imwheel", "remmina", "racket",
    "xorg-utils", "playerctl", "pasystray", "dunst", "otf-fira-code",
    "ttf-mac-fonts", "otf-hermit", "ttf-font-awesome", "ttf-monaco", "tcpdump",
    "ngrep", "wireshark-gtk", "teamviewer", "mopidy-podcast", "tigervnc",
    "kdegraphics-okular", "pandoc", "kdeconnect-git",
]


SERVICES = [
    "sshd.socket", "nvidia-persistenced.service", "NetworkManager.service", "teamviewerd.service", "--user vncserver@:1"
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
    ctx.run("pacaur -S lib32-nvidia steam-libs steam-native")
