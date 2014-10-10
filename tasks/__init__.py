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
    change_shell(ctx)



@ctask
def dotfiles(ctx, flags):
    ctx.run('hash dotfiles || sudo pip install dotfiles')
    return ctx.run('dotfiles -s{1} -R {0}'.format(DOTFILES_DIRECTORY, flags))


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


ns.add_task(change_shell)
ns.add_task(dotfiles)
ns.add_task(setup)
ns.add_task(install_python_libraries)
ns.add_task(vimstall)
