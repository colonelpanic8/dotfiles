from invoke import ctask


@ctask(default=True)
def all(ctx):
    apt_get(ctx)
    get_sbt(ctx)


linux_pacakges = ('zsh', 'tmux', 'emacs24-nox', 'nmap', 'scala', 'default-jdk',
                  'default-jre', 'python-virtualenv', 'htop', 'netcat', 'wget',
                  'zlib1g-dev', 'libxml2-dev', 'libxslt1-dev', 'python-dev',
                  'libncurses5-dev')
@ctask
def apt_get(ctx):
    install_command = 'sudo apt-get -y install'
    for package in linux_pacakges:
        ctx.run('{0} {1}'.format(install_command, package), pty=False)


@ctask
def get_sbt(ctx):
    ctx.run('wget http://dl.bintray.com/sbt/debian/sbt-0.13.5.deb '
            '--output-document=sbt.deb; sudo dpkg -i sbt.deb; '
            'sudo apt-get update; sudo apt-get install sbt; rm sbt.deb')
