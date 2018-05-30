#!/usr/bin/env sh

HANGOUTS_PROFILE="Default"

[ -e $HOME/.hangouts_profile ] && HANGOUTS_PROFILE="$(cat $HOME/.hangouts_profile)"

start_chrome.sh \
	--profile-directory="$HANGOUTS_PROFILE" \
	--app-id=knipolnnllmklapflnccelgolnpehhpl
