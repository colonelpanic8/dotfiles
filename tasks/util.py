import errno
import os

from invoke import run


REPO_DIRECTORY = os.path.dirname(os.path.dirname(__file__))
DOTFILES_DIRECTORY = os.path.join(REPO_DIRECTORY, 'dotfiles')
RESOURCES_DIRECTORY = os.path.join(REPO_DIRECTORY, 'resources')


def command_exists(command, run=run):
    return run("hash {0}".format(command), warn=True, hide=True).exited == 0


def link_filenames(ctx, link_pairs, force=False):
    for source, destination in link_pairs:
        destination = os.path.expanduser(destination)
        source = os.path.expanduser(source)
        if force:
            ctx.run("sudo rm -rf {0}".format(destination))
        if os.path.exists(destination):
            print("Skipping {0} because path already exists".format(destination))
        else:
            print("Linking {0} to {1}".format(destination, source))
            ctx.run('ln -s {0} {1}'.format(source, destination))


def ensure_path_exists(path):
    try:
        os.makedirs(path)
    except OSError as exception:
        if exception.errno != errno.EEXIST:
            raise
