#!/usr/bin/env sh

inputs=$(pactl list sink-inputs | grep 'Sink Input' | grep -Eo '[0-9]*')
selection=$(echo "$inputs" | rofi -dmenu -i)

echo "$inputs" | xargs -I {} -n1 pactl set-sink-input-mute {} 1

pactl set-sink-input-mute "$selection" 0

