{ config, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    steam
    heroic
  ];
  boot.extraModulePackages = with pkgs; [
    xboxdrv
  ];
  hardware.xone.enable = true;
}
