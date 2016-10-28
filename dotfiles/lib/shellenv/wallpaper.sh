WALLPAPER_DIR="$HOME/Pictures/wallpaper/"

random_paper() {
	find "$WALLPAPER_DIR"use -type l | shuf -n1
}

wallpaper() {
	local target_paper=${1:-"$(random_paper)"}
	feh --bg-center $target_paper  --bg-scale "$WALLPAPER_DIR"transparent1x1.png
}

wallpaper_timer() {
	while true; do
		sleep 10m
	done
}
