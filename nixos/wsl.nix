{ config, inputs, pkgs, makeEnable, ... }:
makeEnable config "modules.wsl" false {
  imports = [
    inputs.nixos-wsl.nixosModules.wsl
  ];

  modules.base.enable = false;
  modules.desktop.enable = false;
  modules.xmonad.enable = false;

  wsl = {
    enable = true;
    automountPath = "/mnt";
    startMenuLaunchers = true;
    nativeSystemd = true;
    docker-native.enable = true;
  };
}
