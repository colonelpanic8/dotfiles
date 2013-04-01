# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Customize to your needs...
for file in ~/.{path,exports,prompt,aliases,functions,extra}; do
    [ -r "$file" ] && source "$file"
done

ZSH_THEME="fino"

# Set to this to use case-sensitive completion
CASE_SENSITIVE="true"

# Comment this out to disable bi-weekly auto-update checks
DISABLE_AUTO_UPDATE="true"

plugins=(git github heroku)

source $ZSH/oh-my-zsh.sh

unset file

# COMPLETION SETTINGS
# add custom completion scripts
fpath=(~/.completions $fpath) 
 
# compsys initialization
autoload -U compinit
compinit
