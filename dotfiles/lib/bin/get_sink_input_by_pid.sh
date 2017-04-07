#!/usr/bin/env zsh


get_sink_input_info.hs | jq 'select(.application_process_id == "'"$thePID"'")'
