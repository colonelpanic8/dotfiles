ESSENTIAL=(ipdb ipython virtualenv)
FOR_EDITOR=(jedi pyflakes)
USEFUL=(
    Flask
    ddg
    ouimeaux
    readline
    simplejson
    tox
    numpy
)

function install_python_packages() {
    sudo pip install $ESSENTIAL
    while getopts "uea" OPTCHAR;
    do
        case $OPTCHAR in
            u)
                sudo pip install $USEFUL
                ;;
            e)
                sudo pip install $FOR_EDITOR
                ;;
            a)
                sudo pip install $FOR_EDITOR
                sudo pip install $USEFUL
        esac
    done
}

function install_pygame() {
    sudo pip install numpy
    brew install sdl sdl_image sdl_mixer sdl_ttf portmidi 
    /usr/local/share/python/pip install hg+http://bitbucket.org/pygame/pygame
}
