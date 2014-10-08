REMOTE_CLIPBOARD_PORT='1234'

alias rc_ssh="ssh -R ${REMOTE_CLIPBOARD_PORT}:localhost:1234"

function remote_clipboard_server() {
    while [ 1 ]
    do
        netcat -l -p ${1-$REMOTE_CLIPBOARD_PORT} -e "pbcopy"
    done
}

function remote_clipboard_server_daemon() {
    daemonize `which reattach-to-user-namespace` -l $SHELL -c "source ~/.zshrc; remote_clipboard_server"
}

function _linux_nc_paste_to_remote_clipboard() {
    nc localhost ${1-$REMOTE_CLIPBOARD_PORT} -q 0
}

function _osx_nc_paste_to_remote_clipboard() {
    nc localhost ${1-$REMOTE_CLIPBOARD_PORT} -D
}

function remote_os_copy() {
    if is_osx;
    then
        _osx_nc_paste_to_remote_clipboard

    else
        _linux_nc_write_remote_clipboard
    fi
}

function smart_copy() {
    if is_ssh;
    then
        remote_os_copy
    else
        clipboard
    fi
}
