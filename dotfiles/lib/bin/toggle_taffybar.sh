#!/usr/bin/env sh

dbus-send --print-reply=literal --dest=taffybar.toggle /taffybar/toggle taffybar.toggle.toggleCurrent
