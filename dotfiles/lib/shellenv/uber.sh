# added by newengsetup
export UBER_HOME="$HOME/Uber"
export UBER_OWNER="imalison@uber.com"
[ -z "$GIT_SSH" ] || export GIT_SSH="$HOME/.lib/git-ssh.sh"
export VAGRANT_DEFAULT_PROVIDER=aws
[ -s "/usr/local/bin/virtualenvwrapper.sh" ] && . /usr/local/bin/virtualenvwrapper.sh
[ -s "$HOME/.nvm/nvm.sh" ] && . $HOME/.nvm/nvm.sh

cdsync () {
    cd $(boxer sync_dir $@)
}
editsync () {
    $EDITOR $(boxer sync_dir $@)
}
opensync () {
    open $(boxer sync_dir $@)
}
udir () {
	cd ~/Uber/
}
sdir () {
	cd ~/Uber/sync/
}
