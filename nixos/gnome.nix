{ config, makeEnable, ... }:
makeEnable config "modules.gnome" false {
  services.xserver = {
    desktopManager.gnome.enable = true;
    displayManager.gdm.enable = true;
  };
}
