MAILDIR="$HOME/Mail/"
SYNC_STAMP="$HOME/.mail-sync"
APP_ICON="$(dotfiles_directory)/resources/gmail_logo.png"
TIMEOUT="60"

function mu4e_alert_for_filename {
    local message="$(mu view $1 | grep -B 10 Date)"
    local title="$(echo $message | grep From | sed 's/From: //')"
    local subject="$(echo $message | grep Subject | sed 's/Subject: //')"
    local mu4e_message_id="$(mu4e_get_msg_id_from_file $1)"
    local view_message_command="$(which zsh) -c \"refresh_config && emacs_make_frame_if_none_exists && mu4e_view_message $mu4e_message_id\""
    reattach-to-user-namespace $(which terminal-notifier) \
        -title "$title" \
        -message "$subject" \
        -execute "$view_message_command" \
        -activate "org.gnu.Emacs" \
	-appIcon $APP_ICON
}

function mu4e_update_index_and_alert {
    if test -z "$(find "$MAILDIR" -cnewer "$SYNC_STAMP" -a -type f)"; then
	echo "$(date) - No new messages, skipping alerting and indexing."
    else
	mu4e_update_index
	mu4e_alerts
    fi
}

function mu4e_sync_command {
    local flags=''
    test -z "$*" || flags="-f $@"
    timeout --kill-after "$TIMEOUT" "$TIMEOUT" zsh -c "offlineimap $flags"
}

function mu4e_update_index {
    execute_elisp "(mu4e-update-index)"
}

function mu4e_alerts {
    test -e $SYNC_STAMP || touch $SYNC_STAMP
    touch "${SYNC_STAMP}.in-progress"
    for f in $(find "$MAILDIR" -cnewer "$SYNC_STAMP" -a -type f); do
    	mu4e_alert_for_filename $f
    done
    mv "${SYNC_STAMP}.in-progress" $SYNC_STAMP
}

function mu4e_get_msg_id_from_file {
    mu view $1 -o sexp | grep "message-id" | get_cols ' -1' | sed 's/"//g'
}

function mu4e_view_message_from_file {
    mu4e_view_message "$(mu4e_get_msg_id_from_file $1)"
}

function mu4e_view_message {
    execute_elisp "(progn (email) (mu4e-view-message-with-msgid \"$1\"))"
}
