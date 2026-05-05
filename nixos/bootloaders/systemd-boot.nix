{
  config,
  lib,
  makeEnable,
  ...
}:
makeEnable config "myModules.bootloaders.systemdBoot" true {
  boot.loader = {
    systemd-boot.enable = true;
    efi.canTouchEfiVariables = lib.mkDefault true;
  };
}
