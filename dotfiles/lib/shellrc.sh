for filename in ~/.lib/shellrc/*; do
    source $filename
done
[ -s "/usr/local/bin/virtualenvwrapper.sh" ] && . /usr/local/bin/virtualenvwrapper.sh
[[ -s $(brew --prefix)/etc/profile.d/autojump.sh ]] && . $(brew --prefix)/etc/profile.d/autojump.sh
environment_variable_exists INSIDE_EMACS && inside_emacs_hook
