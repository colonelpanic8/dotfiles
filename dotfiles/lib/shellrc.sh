for filename in ~/.lib/shellrc/*; do
    source $filename
done
environment_variable_exists INSIDE_EMACS && inside_emacs_hook
