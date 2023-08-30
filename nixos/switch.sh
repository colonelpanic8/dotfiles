#!/usr/bin/env sh
sudo nixos-rebuild switch --flake '.#' --impure "$@"
