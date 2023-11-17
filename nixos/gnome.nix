{ config, makeEnable, ... }:
makeEnable config "modules.gnome" false {
  services.xserver = {
    desktopManager.gnome3.enable = true;
    displayManager.gdm.enable = true;
  };
}
