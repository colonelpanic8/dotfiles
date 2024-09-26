{ config, makeEnable, ... }:
makeEnable config "myModules.plasma" true {
  services.displayManager.sddm.enable = true;
  services.xserver = {
    desktopManager.plasma5.enable = true;
  };
}
