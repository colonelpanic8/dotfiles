function add_to_front_of_path {
    export PATH=$@:`echo $PATH | sed "s|:*$@||g" | sed "s|^:||"`
}

function add_to_back_of_path {
    export PATH=`echo $PATH | sed "s|:*$@||g" | sed "s|^:||"`:$@
}

add_to_back_of_path "$HOME/.local/lib/python2.6/site-packages"
add_to_front_of_path "/usr/local/bin"
