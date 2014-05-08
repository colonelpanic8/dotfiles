function add_to_front_of_path {
    export PATH=$@:$(echo $PATH | sed "s|:*$@||g" | sed "s|^:||")
}

function add_to_back_of_path {
    export PATH=$(echo $PATH | sed "s|:*$@||g" | sed "s|^:||"):$@
}

add_to_back_of_path "$HOME/.local/lib/python2.6/site-packages"
hash brew 2>/dev/null && add_to_front_of_path "$(brew --prefix coreutils)/libexec/gnubin"
add_to_front_of_path "/usr/local/bin"

for filename in ~/.lib/shellrc/*; do
    source $filename
done
