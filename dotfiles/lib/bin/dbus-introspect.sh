#!/usr/bin/env sh

dbus-send --session \
  --dest=$1 \
  --type=method_call \
  --print-reply \
  $2 \
  org.freedesktop.DBus.Introspectable.Introspect | tail -n +2 | sed -e "s/^ *string//"
