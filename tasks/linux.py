from invoke import ctask


@ctask(default=True)
def all(ctx):
    apt_get(ctx)


linux_pacakges = ('zsh', 'tmux', 'emacs24-nox', 'nmap', 'scala', 'default-jdk', 'default-jre')
@ctask
def apt_get(ctx):
    install_command = 'sudo apt-get -y install'
    for package in linux_pacakges:
        ctx.run('{0} {1}'.format(install_command, package), pty=False)
