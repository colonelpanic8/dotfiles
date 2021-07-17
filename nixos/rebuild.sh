#!/usr/bin/env sh
nix flake update --impure --inputs-from .
sudo nixos-rebuild switch --flake '.#' --impure
