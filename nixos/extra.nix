{ config, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    # (import ../../Projects/waymonad/default.nix)
    gimp
    android-studio
    texlive.combined.scheme-full
    tor
  ];
}
