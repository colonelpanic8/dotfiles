import os

from invoke import task as ctask

from .util import RESOURCES_DIRECTORY


@ctask(default=True)
def setup(ctx):
    apt_get(ctx)
    get_sbt(ctx)


linux_pacakges = ('zsh', 'tmux', 'emacs24-nox', 'nmap', 'scala', 'default-jdk',
                  'default-jre', 'python-virtualenv', 'htop', 'netcat', 'wget',
                  'zlib1g-dev', 'libxml2-dev', 'libxslt1-dev', 'python-dev',
                  'libncurses5-dev', 'xbindkeys', 'python3-dev', 'xclip',
                  'silversearcher-ag', 'npm', 'xdotool', 'gconf-editor',
                  'dropbox', 'offlineimap', 'mu4e' 'fonts-droid' 'fonts-noto',
                  'gnutls-bin', 'libgmime-2.6-', 'libxapian-dev',
                  'openssh-server', 'golang', 'gitfs',
                  'python-software-properties', 'software-properties-common')


repositories = ('ppa:presslabs/gitfs',)


@ctask
def apt_get(ctx):
    for repository in repositories:
        ctx.run('sudo add-apt-repository {0}'.format(repository))
    ctx.run('sudo apt-get update')
    install_command = 'sudo apt-get -y install'
    for package in linux_pacakges:
        ctx.run('{0} {1}'.format(install_command, package), pty=False)


@ctask
def get_sbt(ctx):
    ctx.run('wget http://dl.bintray.com/sbt/debian/sbt-0.13.5.deb '
            '--output-document=sbt.deb; sudo dpkg -i sbt.deb; '
            'sudo apt-get update; sudo apt-get install sbt; rm sbt.deb')

@ctask
def get_hub(ctx):
    ctx.run("""git clone https://github.com/github/hub.git &&
cd hub &&
./script/build &&
cp hub /usr/bin &&
sudo chown root /usr/bin/hub &&
sudo chmod 777 /usr/bin/hub""")



@ctask
def get_spotify(ctx):
    pass


@ctask
def setup_sources(ctx):
    pass


@ctask
def monaco(ctx):
    ctx.run("sudo cp {0} /usr/share/fonts/fontfiles".format(
        os.path.join(RESOURCES_DIRECTORY, "Monaco-Powerline.otf")
    ))
    ctx.run("sudo fc-cache -fv")
