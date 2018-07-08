{ config, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    gimp
    android-studio
    texlive.combined.scheme-full
    slack
  ];
  boot.extraModulePackages = with pkgs; [ xboxdrv ];
}
