keepass_system_password() {
    keepasshttp.py --get -u "http://$(hostname).systempassword" |
        jq '.[].password' | unescape.py
}

keepass_password() {
    keepasshttp.py --get -u "http://keepass.password" |
        jq '.[].password' | unescape.py
}

my_kp() {
    kpcli --kdb "$HOME/SparkleShare/config/db.kdbx" \
		  --key "$HOME/SparkleShare/config/creds/keepass.key" \
		  --pwfile <(keepass_password) "$@"
}

list_passwords() {
	my_kp --command 'ls Root/' | sed -n '/=== Entries ===/,$p' | tail -n +2
}

get_pass_from_entry() {
	grep -E '^ *Pass:' | get_cols 2
}

select_password() {
	list_passwords | rofi -dmenu -i | get_cols 2
}

get_password() {
	my_kp --command "show -f Root/$1" | get_pass_from_entry
}

echo_selected_password() {
	get_password "$(select_password)"
}

type_selected_password() {
	xdotool type --clearmodifiers "$(echo_selected_password)"
}
