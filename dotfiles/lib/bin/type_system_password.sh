#!/usr/bin/env bash
PASSWORD="$(keepasshttp.py --get -u "http://system.com" | jq '.[].password' | unescape.py)"
sleep .1s
xdotool type "$PASSWORD"
