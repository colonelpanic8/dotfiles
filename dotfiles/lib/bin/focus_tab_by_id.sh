#!/usr/bin/env sh

windowId=$(chromix-too raw chrome.tabs.update "$1" '{"active": true}' | jq .windowId)
chromix-too raw chrome.windows.update "$windowId" '{"focused": true}'

