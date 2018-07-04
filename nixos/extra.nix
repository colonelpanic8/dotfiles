{ config, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    gimp
    texlive.combined.scheme-full
    slack
  ];
  boot.extraModulePackages = with pkgs; [ xboxdrv ];
}
