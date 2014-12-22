MAILDIR="$HOME/Mail/INBOX/"
SYNC_STAMP="$HOME/.mail-sync"
APP_ICON="$(dotfiles_directory)/resources/gmail_logo.png"
SYNC_COMMAND="offlineimap -f INBOX"
TIMEOUT="60"

function mu4e_alert_for_filename {
    local message="$(mu view $1 | grep -B 10 Date)"
    local title="$(echo $message | grep From | sed 's/From: //')"
    local subject="$(echo $message | grep Subject | sed 's/Subject: //')"
    local mu4e_message_id="$(mu4e_get_msg_id_from_file $1)"
    local view_message_command="$(which zsh) -c \"refresh_config && emacs_make_frame_if_none_exists && mu4e_view_message $mu4e_message_id\""
    local app_icon_argument=''
    test -e "$APP_ICON" && app_icon_argument="-appIcon \"$APP_ICON\""
    reattach-to-user-namespace $(which terminal-notifier) \
        -title "$title" \
        -message "$subject" \
        -execute "$view_message_command" \
        -activate "org.gnu.Emacs" \
        $app_icon_argument
}

function mu4e_update_mail {
    timeout $TIMEOUT zsh -c "_mu4e_update_mail"
}

function _mu4e_update_mail {
    eval $SYNC_COMMAND
    if test -z "$(find "$MAILDIR" -cnewer "$SYNC_STAMP" -a -type f)"; then
	echo "$(date) - No new messages, skipping alerting and indexing."
    else
	mu4e_update_index
	mu4e_alerts
    fi
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
    execute_elisp "(mu4e-view-message-with-msgid \"$1\")"
}
