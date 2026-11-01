#!/usr/bin/env sh


desktop_dmenu --prompt-only -p "pkill -9" | xargs pkill -9
