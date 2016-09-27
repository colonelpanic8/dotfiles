#!/usr/bin/env bash

WALLPAPER_DIR="$HOME/Pictures/wallpaper/"

random_paper() {
	find "$WALLPAPER_DIR"use -type f | shuf -n1
}

wallpaper() {
	while true; do
		feh --bg-center $(random_paper) --bg-scale "$WALLPAPER_DIR"transparent1x1.png
		sleep 1m
	done
}

wallpaper
