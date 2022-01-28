{ config, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    # steam
  ];
  boot.extraModulePackages = with pkgs; [ xboxdrv ];
}
