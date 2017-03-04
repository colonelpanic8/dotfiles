#!/usr/bin/env sh

actual="$(cat /sys/class/backlight/intel_backlight/actual_brightness)"
max="$(cat /sys/class/backlight/intel_backlight/max_brightness)"

temp="$(( $actual * 100 ))"
percentage="$(( $temp/$max ))"

volnoti-show "$percentage"
