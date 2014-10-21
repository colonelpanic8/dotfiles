import os

from invoke import ctask

from .util import RESOURCES_DIRECTORY, command_exists


@ctask(default=True)
def all(ctx):
    get_command_line_tools(ctx)
    get_brew(ctx)
    brew_install(ctx)
    brew_cask(ctx)
    osx_config(ctx)


ESSENTIAL = (
    "emacs --cocoa --srgb --with-x", "tmux", "python --with-brewed-openssl", "htop", "zsh", "make",
    "macvim --override-system-vim --custom-system-icons --with-features=huge "
    "--enable-rubyinterp --enable-pythoninterp --enable-perlinterp --enable-cscope"
)
BASICS=(
    "findutils", "coreutils", "binutils", "diffutils", "ed --default-names",
    "gawk", "gnu-indent --default-names", "gnu-sed --default-names",
    "gnu-tar --default-names", "gnu-which --default-names", "gnutls --default-names",
    "grep --default-names", "gzip", "watch", "wdiff --with-gettext", "wget --enable-iri"
)
SHOULD_INSTALL = (
    "nmap", "readline", "netcat", "reattach-to-user-namespace", "daemonize",
    "ngrep", "gist", "gawk", "pstree", "ack", "hub", "tig", "heroku", "scala",
    "sbt", "node", "npm"
)
MISC = ("file-formula", "less", "openssh --with-brewed-openssl",
        "perl518", "rsync", "svn", "unzip", "docker", "boot2docker", "pandoc",
        "mercurial")

CASKS = ('alfred', 'caffeine', 'flux', 'google-chrome', 'iterm2', 'spotify', 'vlc', 'virtualbox', 'xquartz')
@ctask
def osx_config(ctx):
    ctx.run('source {0}; osx_config'.format(os.path.join(RESOURCES_DIRECTORY, 'osx.sh')))

@ctask
def brew_cask(ctx):
    ctx.run('brew install caskroom/cask/brew-cask')
    for cask in CASKS:
        ctx.run('brew cask install {0}'.format(cask))

@ctask
def get_brew(ctx):
    if not command_exists('brew'):
        ctx.run('ruby -e "$(curl -fsSL https://raw.github.com/Homebrew/homebrew/go/install)"')

@ctask
def brew_install(ctx):
    for package_name in ESSENTIAL + BASICS + SHOULD_INSTALL + MISC:
        ctx.run('brew install {0}'.format(package_name))

@ctask
def setup_emacs(ctx):
    ctx.run('ln -s $(brew --prefix emacs) /Applications/emacs')


@ctask
def get_command_line_tools(ctx):
    if not command_exists('gcc'):
        ctx.run('xcode-select --install')
