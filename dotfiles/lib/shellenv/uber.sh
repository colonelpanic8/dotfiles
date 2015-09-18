# added by newengsetup
export UBER_HOME="$HOME/Uber"
export UBER_OWNER="imalison@uber.com"
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

function merchant_curl {
	tcurl -p localhost:4338 merchant Merchant::$1 -3 "$2" -t ~/go/src/code.uber.internal/everything/merchant/thrift
}
