{ config, inputs, pkgs, makeEnable, ... }:
makeEnable config "modules.wsl" false {
  imports = [
    inputs.nixos-wsl.nixosModules.wsl
  ];

  modules.base.enable = false;
  modules.desktop.enable = false;
  modules.xmonad.enable = false;
  modules.plasma.enable = false;

  # Update timezone automatically
  services.tzupdate.enable = true;

  wsl.wslConf.automount.root = "/mnt";

  wsl = {
    enable = true;
    startMenuLaunchers = true;
    nativeSystemd = true;
  };
}
