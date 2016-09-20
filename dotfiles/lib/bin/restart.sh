#!/usr/bin/env sh
pkill "$1"
nohup "$@" & >"/tmp/$1.out" 2>"/tmp/$1.error"
