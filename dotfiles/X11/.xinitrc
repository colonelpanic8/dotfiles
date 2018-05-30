# -*- mode: sh -*-
xrdb -merge "$HOME/.Xresources"

# Disable screensaver
setterm -blank 0 -powersave off -powerdown 0
xset s off

# Increase the repeat rate
xset r rate 200 30
