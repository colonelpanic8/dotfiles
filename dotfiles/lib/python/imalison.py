import os
import errno
from invoke import run, Collection, task as ctask


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


def command_exists(command, run=run):
    return run("hash {0}".format(command), warn=True, hide=True).exited == 0


def build_task_factory(ns):
    def task(function):
        ns.add_task(ctask(function))
        return function
    return task


def namespace_and_factory():
    ns = Collection()
    return ns, build_task_factory(ns)


def extension_checker(extension):
    extension_suffix = ".{}".format(extension)
    def ends_with(string):
        return string.endswith(extension_suffix)
    return ends_with


def tasks_from_directory(directory_path, file_predicate=extension_checker("sh")):
    ns, make_task = namespace_and_factory()

    def task_from_file(filepath):
        @make_task()
        def run_script(ctx):
            ctx.run(filepath)
        return run_script

    filepaths = filter(os.path.isfile,
                       [os.path.join(directory_path, filename)
                        for filename in os.listdir(directory_path)])

    map(task_from_file, filepaths)
    return ns
