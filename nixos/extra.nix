{ config, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    texlive.combined.scheme-full
  ];
  boot.extraModulePackages = with pkgs; [ xboxdrv ];
}
