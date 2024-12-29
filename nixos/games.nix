{ config, pkgs, makeEnable, ... }:
makeEnable config "myModules.games" false {
  environment.systemPackages = with pkgs; [
    steam
    # heroic
  ];
  hardware.xone.enable = true;
}
