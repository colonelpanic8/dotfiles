export ANTIGEN_PATH="$HOME/.antigen.zsh"

function upgrade_antigen {
	curl -L https://raw.githubusercontent.com/zsh-users/antigen/master/antigen.zsh > $ANTIGEN_PATH
}

test -e $ANTIGEN_PATH || upgrade_antigen

source $ANTIGEN_PATH

antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle git

antigen apply
