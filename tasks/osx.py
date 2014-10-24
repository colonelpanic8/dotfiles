import os

from invoke import ctask

from . import util


@ctask(default=True)
def all(ctx):
    get_command_line_tools(ctx)
    get_brew(ctx)
    brew_install(ctx)
    brew_cask(ctx)
    setup_cocoa_emacs(ctx)
    enable_access_for_assistive_devices(ctx)
    enable_hyper(ctx)
    osx_config(ctx)


macvim_install = ("macvim --override-system-vim --custom-system-icons "
                  "--with-features=huge --enable-rubyinterp "
                  "--enable-pythoninterp --enable-perlinterp --enable-cscope")
ESSENTIAL = (
    "emacs --cocoa --srgb --with-x", "tmux", "python --with-brewed-openssl",
    "htop", "zsh", "make", macvim_install
)
BASICS = (
    "findutils", "coreutils", "binutils", "diffutils", "ed --default-names",
    "gawk", "gnu-indent --default-names", "gnu-sed --default-names",
    "gnu-tar --default-names", "gnu-which --default-names",
    "gnutls --default-names", "grep --default-names", "gzip", "watch",
    "wdiff --with-gettext", "wget --enable-iri"
)
SHOULD_INSTALL = (
    "nmap", "readline", "netcat", "reattach-to-user-namespace", "daemonize",
    "ngrep", "gist", "gawk", "pstree", "ack", "hub", "tig", "heroku", "scala",
    "sbt", "node", "npm"
)
MISC = ("file-formula", "less", "openssh --with-brewed-openssl",
        "perl518", "rsync", "svn", "unzip", "docker", "boot2docker", "pandoc",
        "mercurial")
CASKS = ('caffeine', 'flux', 'google-chrome', 'iterm2', 'spotify', 'synergy',
         'virtualbox', 'xquartz', 'slate', 'java', 'vlc', 'seil', 'karabiner')

@ctask
def osx_config(ctx):
    ctx.run('sudo {0}'.format(
        os.path.join(util.RESOURCES_DIRECTORY, 'osx.sh')
    ), pty=True)


@ctask
def brew_cask(ctx):
    ctx.run('brew install caskroom/cask/brew-cask')
    for cask in CASKS:
        ctx.run('brew cask install {0}'.format(cask))


@ctask
def get_brew(ctx):
    if not util.command_exists('brew'):
        ctx.run('ruby -e "$(curl -fsSL https://raw.github.com/Homebrew/homebrew/go/install)"')

@ctask
def brew_install(ctx):
    for package_name in ESSENTIAL + BASICS + SHOULD_INSTALL + MISC:
        ctx.run('brew install {0}'.format(package_name))


@ctask
def setup_cocoa_emacs(ctx):
    if not os.path.exists('/Applications/Emacs.app'):
        ctx.run('ln -s $(brew --prefix emacs)/Emacs.app /Applications/Emacs.app', hide=True)

    launch_agent_dir = os.path.expanduser('~/Library/LaunchAgents/')
    filename = 'set-path.plist'

    source = os.path.join(util.RESOURCES_DIRECTORY, filename)
    destination = os.path.join(launch_agent_dir, filename)

    if os.path.exists(source) and not os.path.exists(destination):
        util.ensure_path_exists(launch_agent_dir)
        ctx.run('ln -s {0} {1}'.format(source, destination))


APPS_NEEDING_ASSISTIVE_DEVICE_ACCESS = ('Slate', 'Synergy', 'iTerm')
@ctask
def enable_access_for_assistive_devices(ctx):
    for app in APPS_NEEDING_ASSISTIVE_DEVICE_ACCESS:
        app_string = '/Applications/{0}.app'.format(app)
        user_application = os.path.expanduser('~' + app_string)
        enable_access_if_exists(ctx, user_application)
        enable_access_if_exists(ctx, app_string)
        enable_access_if_exists("/Applications/Karabiner.app/Contents/Applications/Karabiner_AXNotifier.app")


def enable_access_if_exists(ctx, app_string):
    if os.path.exists(app_string):
        ctx.run(
            'zsh -c "source ~/.zshrc && '
            'enable_access_for_assistive_devices \"{0}\""'.format(
                app_string
            )
        )

@ctask
def enable_hyper(ctx):
    source = '{0}/karabiner-hyper.xml'.format(util.RESOURCES_DIRECTORY)
    destination = os.path.expanduser("~/Library/Application\\ Support/Karabiner/private.xml")
    try:
        ctx.run("rm {0}".format(destination))
    except:
        pass
    ctx.run("ln -s {0} {1}".format(source, destination))
    ctx.run("{0}/karabiner_config.sh".format(util.RESOURCES_DIRECTORY))

@ctask
def get_command_line_tools(ctx):
    if not util.command_exists('gcc'):
        ctx.run('xcode-select --install')
