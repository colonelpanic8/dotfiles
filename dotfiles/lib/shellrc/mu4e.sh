MAILDIR="$HOME/Mail"
SYNC_STAMP="$HOME/.mail-sync"

function mu4e_alert_for_filename {
    local message="$(mu view $1 | grep -B 10 Date)"
    local title="$(echo $message | grep From | sed 's/From: //')"
    local subject="$(echo $message | grep Subject | sed 's/Subject: //')"
    local view_file_command="$(which zsh) -c \"refresh_config && mu4e_view_message_from_file $1\""
    reattach-to-user-namespace $(which terminal-notifier) \
	-title "$title" \
	-message "$subject" \
	-execute "$view_file_command" \
	-activate "org.gnu.Emacs" \
	-appIcon "$(dotfiles_directory)/resources/gmail_logo.png"
}

function mu4e_update_mail {
    offlineimap
    mu4e_update_index
    mu4e_alerts
}

function mu4e_update_index {
    execute_elisp "(mu4e-update-index)"
}

function mu4e_alerts {
    test -e $SYNC_STAMP || touch $SYNC_STAMP
    touch "${SYNC_STAMP}.in-progress"
    for f in $(find "$MAILDIR/INBOX" -cnewer "$SYNC_STAMP" -a -type f); do
    	mu4e_alert_for_filename $f
    done
    mv "${SYNC_STAMP}.in-progress" $SYNC_STAMP
}

function mu4e_get_msg_id_from_file {
    mu view $1 -o sexp | grep "message-id" | get_cols ' -1'
}

function mu4e_view_message_from_file {
    mu4e_view_message "$(mu4e_get_msg_id_from_file $1)"
}

function mu4e_view_message {
    local elisp="(mu4e-view-message-with-msgid "$1")"
    execute_elisp "$elisp"
}
