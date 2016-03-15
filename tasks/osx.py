import os

from invoke import ctask

from . import util


@ctask(default=True)
def setup(ctx):
    brew(ctx)
    cider(ctx)
    access_for_assistive_devices(ctx)
    karabiner(ctx)
    locate(ctx)
    set_path_for_launchd(ctx)
    rvm(ctx)
    fonts(ctx)
    fix_htop(ctx)
    iTerm(ctx)
    keyboard_settings(ctx)
    custom_keyboard_shortcuts(ctx)


@ctask
def macvim(ctx):
    macvim_install = (
        "macvim --override-system-vim --custom-system-icons "
        "--with-features=huge --enable-rubyinterp --enable-pythoninterp "
        "--enable-perlinterp --enable-cscope"
    )
    ctx.run("brew install {0}".format(macvim_install))
    ctx.run("vim +BundleInstall! +q +q")


@ctask
def setup_dbus(ctx):
    ctx.run("ln -sfv /usr/local/opt/d-bus/*.plist ~/Library/LaunchAgents")


@ctask
def system_settings(ctx):
    ctx.run('{0}'.format(
        os.path.join(util.RESOURCES_DIRECTORY, 'osx_setup.sh')
    ), pty=True)


@ctask
def cider(ctx):
    ctx.run('brew install caskroom/cask/brew-cask')
    if not util.command_exists('cider'):
        ctx.run('sudo pip install cider')
    ctx.run('cider restore -i')


@ctask
def brew(ctx):
    path = 'https://raw.githubusercontent.com/Homebrew/install/master/install)'
    if not util.command_exists('brew'):
        ctx.run('ruby -e "$(curl -fsSL {0}'.format(path))


@ctask
def set_path_for_launchd(ctx):
    launch_agent_dir = os.path.expanduser('~/Library/LaunchAgents/')
    filename = 'set-path.plist'
    source = os.path.join(util.RESOURCES_DIRECTORY, filename)
    destination = os.path.join(launch_agent_dir, filename)

    if os.path.exists(source) and not os.path.exists(destination):
        util.ensure_path_exists(launch_agent_dir)
        ctx.run('ln -s {0} {1}'.format(source, destination))


APPS_NEEDING_ASSISTIVE_DEVICE_ACCESS = ('Slate', 'Synergy', 'iTerm')


@ctask
def access_for_assistive_devices(ctx):
    for app in APPS_NEEDING_ASSISTIVE_DEVICE_ACCESS:
        app_string = '/Applications/{0}.app'.format(app)
        user_application = os.path.expanduser('~' + app_string)
        access_if_exists(ctx, user_application)
        access_if_exists(ctx, app_string)
        access_if_exists(
            ctx,
            "/Applications/Karabiner.app/"
        )
        access_if_exists(
            ctx,
            "/Applications/Karabiner.app/Contents/Applications/"
            "Karabiner_AXNotifier.app"
        )


def access_if_exists(ctx, app_string):
    if os.path.exists(app_string):
        print("enabling access for {0}", app_string)
        ctx.run(
            'zsh -c "source ~/.zshrc && '
            'enable_access_for_assistive_devices \"{0}\""'.format(
                app_string
            )
        )


@ctask(aliases=['hyper', 'fast_repeat'])
def karabiner(ctx):
    source = '{0}/karabiner-hyper.xml'.format(util.RESOURCES_DIRECTORY)
    destination_folder = os.path.join(
        os.path.expanduser("~/Library"), "Application\\ Support", "Karabiner"
    )
    destination = os.path.join(destination_folder, "private.xml")
    try:
        ctx.run("rm {0}".format(destination))
    except:
        pass
    util.ensure_path_exists(destination_folder)
    ctx.run("ln -s {0} {1}".format(source, destination))
    ctx.run("{0}/karabiner_config.sh".format(util.RESOURCES_DIRECTORY))


@ctask
def locate(ctx):
    ctx.run('sudo launchctl load -w '
            '/System/Library/LaunchDaemons/com.apple.locate.plist')


@ctask
def rvm(ctx):
    ctx.run('\\curl -sSL https://get.rvm.io | bash -s stable')


@ctask
def fonts(ctx):
    ctx.run('open {0}'.format(
        os.path.join(util.RESOURCES_DIRECTORY, 'fonts', "*"))
    )


@ctask
def cl_tools(ctx):
    if not util.command_exists('gcc'):
        ctx.run('xcode-select --install')


@ctask
def fix_htop(ctx):
    real_htop_location = ctx.run(
        "zsh -c 'greadlink -f $(brew --prefix htop-osx)'"
    ).stdout.strip() + "/bin/htop"
    ctx.run("sudo chmod 6555 {0}".format(real_htop_location))
    ctx.run("sudo chown root {0}".format(real_htop_location))


@ctask
def iTerm(ctx):
    library_plist = os.path.join(os.path.expanduser("~"), "Library",
                                 "Preferences", "com.googlecode.iterm2.plist")
    ctx.run("defaults write {0} LoadPrefsFromCustomFolder -bool true".format(
        library_plist
    ))
    ctx.run("defaults write {0} PrefsCustomFolder -string {1}".format(
        library_plist, util.RESOURCES_DIRECTORY
    ))


@ctask
def launch_agents(ctx, flags=''):
    ctx.run('dotfiles -sn{1} -R {0}/resources/LaunchAgents/ '
            '-H ~/Library/LaunchAgents'.format(util.REPO_DIRECTORY, flags))


@ctask
def keyboard_settings(ctx):
    ctx.run("zsh -c 'refresh_config && set_modifier_keys_on_all_keyboards'")


settings_directory = os.path.join(util.RESOURCES_DIRECTORY, 'osx_settings')
all_save_settings = []
all_write_settings = []
all_diff_settings = []


def functions_for_filename(filename):
    filepath = os.path.join(settings_directory, filename)
    task_name = 'settings-write:' + filename.replace('.', '-')
    @ctask(name=task_name)
    def task(ctx):
        ctx.run("defaults write {0} '$(cat {1})'".format(
            filename, filepath
        ))
    globals()[task_name] = task
    all_write_settings.append(task)

    task_name = 'settings-save:' + filename.replace('.', '-')
    @ctask(name=task_name)
    def task(ctx):
        ctx.run("defaults read {0} > {1}".format(
            filename, filepath
        ))
    globals()[task_name] = task
    all_save_settings.append(task)

    task_name = 'settings-diff:' + filename.replace('.', '-')
    @ctask(name=task_name)
    def task(ctx):
        print filepath
        print filename
        ctx.run("zsh -c 'icdiff <(defaults read {0}) {1}'".format(
            filename, filepath
        ))
    globals()[task_name] = task
    all_diff_settings.append(task)


for _, _, filenames in os.walk(settings_directory):
    for filename in filenames:
        functions_for_filename(filename)


@ctask
def settings_write_all(ctx):
    for function in all_write_settings:
        function(ctx)


@ctask
def custom_keyboard_shortcuts(ctx):
    command_string = """defaults write -globalDomain NSUserKeyEquivalents '{"Enter Full Screen" = "@\U21a9";"Exit Full Screen" = "@\U21a9";"Full Screen" = "@\U21a9";}'"""
    ctx.run(command_string)
