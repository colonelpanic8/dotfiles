#!/usr/bin/env zsh

chromix-too raw chrome.windows.getLastFocused '{"populate": true}'  | jq -cM '.tabs[]' | jq 'select(.active)' | jq .id | xargs split_tab_by_id.sh
