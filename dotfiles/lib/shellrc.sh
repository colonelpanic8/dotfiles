function add_to_front_of_path {
    export PATH=$@:$(echo $PATH | sed "s|:*$@||g" | sed "s|^:||")
}

function add_to_back_of_path {
    export PATH=$(echo $PATH | sed "s|:*$@||g" | sed "s|^:||"):$@
}

add_to_back_of_path "$HOME/.local/lib/python2.6/site-packages"
hash brew 2>/dev/null && add_to_front_of_path "$(brew --prefix coreutils)/libexec/gnubin"
add_to_front_of_path "/usr/local/bin"
hash brew 2>/dev/null && add_to_front_of_path "$(brew --prefix emacs)/libexec/gnubin"

for filename in ~/.lib/shellrc/*; do
    source $filename
done

if is_osx; then
    export CFLAGS=-Qunused-arguments
    export CPPFLAGS=-Qunused-arguments
    add_to_back_of_path "/System/Library/PrivateFrameworks/Apple80211.framework/Versions/Current/Resources/"
    export JAVA_HOME="$(find /Library/Java/JavaVirtualMachines -depth 1 | head -n 1)/Contents/Home"
    # Access gnu man pages.
    hash brew 2> /dev/null && export MANPATH="$(brew --prefix)/opt/coreutils/libexec/gnuman:$MANPATH"
else 
    export JAVA_HOME="$(update-alternatives --config java | get_cols ' -1' | head -n 1)"
fi

add_to_front_of_path "$JAVA_HOME/bin"

add_to_back_of_path "$(dotfiles_directory)/resources/python"
add_to_back_of_path "/usr/local/sbin"
