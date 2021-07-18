#!/usr/bin/env sh
rm flake.lock
nix flake update --impure --inputs-from .
sudo nixos-rebuild switch --flake '.#' --impure
