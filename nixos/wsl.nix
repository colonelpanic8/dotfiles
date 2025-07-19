{ config, inputs, pkgs, makeEnable, ... }:
makeEnable config "myModules.wsl" false {
  imports = [
    inputs.nixos-wsl.nixosModules.wsl
  ];

  myModules.base.enable = false;
  myModules.desktop.enable = false;
  myModules.xmonad.enable = false;
  myModules.plasma.enable = false;

  services.tzupdate.enable = true;

  wsl.wslConf.automount.root = "/mnt";

  wsl = {
    enable = true;
    startMenuLaunchers = true;
  };
}
