# added by newengsetup
export UBER_HOME="$HOME/Uber"
export UBER_OWNER="imalison@uber.com"
export UBER_LDAP_UID="imalison"
[ -z "$GIT_SSH" ] || export GIT_SSH="$HOME/.lib/git-ssh.sh"
export VAGRANT_DEFAULT_PROVIDER=aws
[ -s "/usr/local/bin/virtualenvwrapper.sh" ] && . /usr/local/bin/virtualenvwrapper.sh

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
function uc {
	udir
	local project_dir="$(basename $1)"
	if [ ! -e $project_dir ]; then
		git clone gitolite@code.uber.internal:$1
	fi
	cd "$project_dir"
}

function urtc {
	uc "rt/$1"
}

goclone() {
  mkdir -p $GOPATH/src/code.uber.internal/$1
  git clone gitolite@code.uber.internal:$1 $GOPATH/src/code.uber.internal/$1
}

alias hyper_tunnel='ssh -fN -L 21300:hyperbahn01-sjc1:21300 adhoc03-sjc1'

add_to_path "$HOME/bin" --before

alias land_current='arc land $(git which-branch)'

alias kill_h_forward='ps aux | grep ssh | grep 21300 | get_cols 2 | xargs kill -9'
