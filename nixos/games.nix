{ config, pkgs, makeEnable, ... }:
makeEnable config "myModules.games" false {
  environment.systemPackages = with pkgs; [
    steam
    # heroic
  ];
  boot.extraModulePackages = with pkgs; [
    xboxdrv
  ];
  hardware.xone.enable = true;
}
