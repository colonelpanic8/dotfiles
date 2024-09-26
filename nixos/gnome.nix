{ config, makeEnable, ... }:
makeEnable config "myModules.gnome" false {
  services.xserver = {
    desktopManager.gnome.enable = true;
    displayManager.gdm.enable = true;
  };
}
