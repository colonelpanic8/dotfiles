import os

from invoke import task

from .util import RESOURCES_DIRECTORY


ARCH_PACKAGES = [
    "adobe-source-code-pro-fonts", "lib32-alsa-lib", "synergy", "git",
    "pkg-config", "pyenv", "rbenv", "alsa-utils", "patch", "spotify",
    "google-chrome", "autoconf", "automake", "cask", "emacs-git", "xmobar",
    "the_silver_searcher", "jdk8-openjdk", "openjdk8-doc", "openjdk8-src",
    "scala", "clojure", "go", "ruby", "node", "ghc", "rust", "nodejs", "nvm",
    "nvidia-settings", "gnome-tweak-tool", "screenfetch", "htop", "tmux",
    "texlive-most", "leiningen", "boot", "gnome-settings-daemon"
]


@task
def install_pacaur(ctx):
    ctx.run(os.path.join(RESOURCES_DIRECTORY, "install_pacaur.sh"))
