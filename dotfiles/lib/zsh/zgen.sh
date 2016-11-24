export ZGEN_PATH="$HOME/.zgen"

test -e $ZGEN_PATH || git clone https://github.com/tarjoilija/zgen.git $ZGEN_PATH

source "$ZGEN_PATH/zgen.zsh"

# if the init scipt doesn't exist
if ! zgen saved; then
	zgen oh-my-zsh
	zgen oh-my-zsh plugins/git
	zgen oh-my-zsh plugins/sudo
	zgen oh-my-zsh plugins/command-not-found
	zgen oh-my-zsh plugins/pip
	zgen load zsh-users/zsh-syntax-highlighting
	zgen load kennethreitz/autoenv

	# generate the init script from plugins above
	zgen save
fi
