source ~/.lib/shellenv/functions.sh

add_to_back_of_path "$HOME/.local/lib/python2.6/site-packages"
add_to_back_of_path "$HOME/.rvm/bin"
add_to_front_of_path "$HOME/bin"
hash brew 2>/dev/null && add_to_front_of_path "$(brew --prefix coreutils)/libexec/gnubin"
add_to_front_of_path "/usr/local/bin"
hash brew 2>/dev/null && add_to_front_of_path "$(brew --prefix emacs)/libexec/gnubin"

if is_osx; then
    export CFLAGS=-Qunused-arguments
    export CPPFLAGS=-Qunused-arguments
    add_to_back_of_path "/System/Library/PrivateFrameworks/Apple80211.framework/Versions/Current/Resources"
    local JDK_LOCATION="$(find /Library/Java/JavaVirtualMachines -depth 1 | head -n 1)"
    export JAVA_HOME="$JDK_LOCATION/Contents/Home"
    export STUDIO_JDK=$JDK_LOCATION
    export GRADLE_HOME="$(brew --prefix gradle)"
    export ANDROID_HOME="$(brew --prefix android-sdk)"
    add_to_back_of_path "$ANDROID_HOME"
    
    # Access gnu man pages.
    hash brew 2> /dev/null && export MANPATH="$(brew --prefix)/opt/coreutils/libexec/gnuman:$MANPATH"
else 
    export JAVA_HOME="$(update-alternatives --config java | get_cols ' -1' | head -n 1)"
    is_osx && export VISUAL="which emacsclient -c -n"
fi

add_to_front_of_path "$JAVA_HOME/bin"

add_to_back_of_path "$(dotfiles_directory)/resources/python"
add_to_back_of_path "/usr/local/sbin"

# Load RVM into a shell session *as a function*
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"

for filename in ~/.lib/shellenv/*; do
    source $filename
done

function with_shellrc {
    zsh -c "source ~/.zshrc && ""$@"
}

# Travis completion
[ -f "$HOME/.travis/travis.sh" ] && source "$HOME/.travis/travis.sh"

test -e /usr/libexec/path_helper && eval `/usr/libexec/path_helper -s`


export NVM_DIR="/Users/imalison/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm
export NODE_PATH="/usr/local/lib/node_modules/"

add_to_front_of_path "$HOME/.lib/python" 'PYTHONPATH'

export RBENV_ROOT=/usr/local/var/rbenv
add_to_front_of_path "$HOME/.rbenv/bin"
hash rbenv 2> /dev/null && eval "$(rbenv init -)"
