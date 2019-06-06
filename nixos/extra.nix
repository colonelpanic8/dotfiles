{ config, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    gimp
    android-studio
    texlive.combined.scheme-full
    tor
  ];
}
