#!/bin/bash

# If parent command is "npm ...", then redirect stderr to /dev/null
# https://github.com/npm/npm/issues/7979#issuecomment-94953923

GRANDPARENT_PID=$(ps -p $PPID -o ppid=)

if ps -p $GRANDPARENT_PID -o command= | grep -q -P "^npm"; then
  ssh "$@" 2>/dev/null
else
  ssh "$@"
fi
