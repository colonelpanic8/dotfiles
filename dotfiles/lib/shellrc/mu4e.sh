MAILDIR="$HOME/Mail"
DBUS_COOKIE="$HOME/.sauron-dbus"
SYNC_STAMP="$HOME/.mail-sync"

function sauron_msg {
    if test -z "$DBUS_SESSION_BUS_ADDRESS"; then
	echo "DBUS session not found."
        if test -e $DBUS_COOKIE; then
            export DBUS_SESSION_BUS_ADDRESS="`cat $DBUS_COOKIE`"
	else
	    echo "DBUS cookie not found, unable to send message"
        fi
    fi
    if test -n "$DBUS_SESSION_BUS_ADDRESS"; then
        dbus-send --session                          \
            --dest="org.gnu.Emacs"                   \
            --type=method_call                       \
            "/org/gnu/Emacs/Sauron"                  \
            "org.gnu.Emacs.Sauron.AddMsgEvent"       \
            string:"$1" uint32:3 string:"$2"
    fi
}

function update_mail {
    offlineimap
    touch "${SYNC_STAMP}.in-progress"
    for f in `find "$MAILDIR" -cnewer $SYNC_STAMP -a -type f`; do
	local message="$(mu view $f | grep -B 10 Date)"
	test -n "$message" && sauron_msg "Gmail" "$message"
    done
    mv "${SYNC_STAMP}.in-progress" $SYNC_STAMP
}
