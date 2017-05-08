#!/usr/bin/env zsh

echo $1
chromix-too raw chrome.windows.create '{"tabId": '"$1}"
