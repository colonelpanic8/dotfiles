#!/usr/bin/env zsh

rm -rf ~/.cache/taffybar && cd ~/.config/taffybar && ghc taffybar.hs
./taffybar &
pkill -9 taffybar
systemctl --user restart taffybar.service
