REMOTE_CLIPBOARD_PORT='1234'

function remote_clipboard_server() {
    while [ 1 ]
    do
        netcat -l -p ${1-$REMOTE_CLIPBOARD_PORT} -e "pbcopy"
    done
}

function remote_clipboard_server_daemon() {
    daemonize `which reattach-to-user-namespace` -l $SHELL -c "source ~/.zshrc; remote_clipboard_server"
}

function linux_nc_paste_to_remote_clipboard() {
    nc localhost ${1-$REMOTE_CLIPBOARD_PORT} -q 0
}

function osx_nc_paste_to_remote_clipboard() {
    nc localhost ${1-$REMOTE_CLIPBOARD_PORT} -D
}
