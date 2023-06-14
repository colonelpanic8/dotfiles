{ config, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    steam
    heroic
  ];
  boot.extraModulePackages = with pkgs; [
    xboxdrv
  ];
  # hardware.xpadneo.enable = true;
  hardware.xone.enable = true;
}
