import os
import sys

from invoke import Collection, ctask

from . import osx
from . import linux
from .util import DOTFILES_DIRECTORY, RESOURCES_DIRECTORY


ns = Collection()
ns.add_collection(osx)
ns.add_collection(linux)


@ctask(default=True)
def setup(ctx):
    ctx.config['run']['pty'] = False
    ctx.config['run']['warn'] = True
    if 'darwin' in sys.platform:
        osx.all(ctx)
    else:
        linux.all(ctx)
    dotfiles(ctx, 'f')
    install_python_libraries(ctx)
    powerline(ctx)
    install_npm_libraries(ctx)
    change_shell(ctx)



@ctask
def dotfiles(ctx, flags=''):
    ctx.run('hash dotfiles || sudo pip install dotfiles')
    return ctx.run('dotfiles -s{1} -R {0}'.format(DOTFILES_DIRECTORY, flags))


@ctask
def powerline(ctx):
    ctx.run('pip install --user git+git://github.com/Lokaltog/powerline')


@ctask
def install_python_libraries(ctx):
    ctx.run('sudo pip install -r {0}'.format(
        os.path.join(RESOURCES_DIRECTORY, 'requirements.txt')
    ))


@ctask
def install_npm_libraries(ctx):
    ctx.run('source {0}'.format(
        os.path.join(RESOURCES_DIRECTORY, 'npm.sh')
    ))


@ctask
def vimstall(ctx):
    ctx.run('vim +BundleInstall! +q +q')


@ctask
def change_shell(ctx):
    ctx.run('sudo chsh -s $(which zsh) $(whoami)')


@ctask
def customize_user_settings(ctx):
    input_function = input if sys.version_info.major == 3 else raw_input
    username = input_function("Enter the user's full name")
    email = input_function("Enter the user's Email.")
    with os.path.expanduser('~/.gitconfig.custom', 'w') as custom_file:
        custom_file.write("""[user]
        name = {0}
	email = {1}""".format(username, email))


ns.add_task(change_shell)
ns.add_task(customize_user_settings)
ns.add_task(dotfiles)
ns.add_task(install_npm_libraries)
ns.add_task(install_python_libraries)
ns.add_task(powerline)
ns.add_task(setup)
ns.add_task(vimstall)
