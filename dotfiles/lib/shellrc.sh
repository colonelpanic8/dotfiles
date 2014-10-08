function add_to_front_of_path {
    export PATH=$@:$(echo $PATH | sed "s|:*$@||g" | sed "s|^:||")
}

function add_to_back_of_path {
    export PATH=$(echo $PATH | sed "s|:*$@||g" | sed "s|^:||"):$@
}

add_to_back_of_path "$HOME/.local/lib/python2.6/site-packages"
hash brew 2>/dev/null && add_to_front_of_path "$(brew --prefix coreutils)/libexec/gnubin"
add_to_front_of_path "/usr/local/bin"

add_to_back_of_path "/System/Library/PrivateFrameworks/Apple80211.framework/Versions/Current/Resources/"

for filename in ~/.lib/shellrc/*; do
    source $filename
done

if is_osx; then
    export CFLAGS=-Qunused-arguments
    export CPPFLAGS=-Qunused-arguments
fi

add_to_back_of_path "$(dotfiles_directory)/resources/python"

# Access gnu man pages.
MANPATH="$HOMEBREW_PREFIX/opt/coreutils/libexec/gnuman:$MANPATH"
