{ config, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    gimp
    texlive.combined.scheme-full
  ];
  boot.extraModulePackages = with pkgs; [ xboxdrv ];
}
