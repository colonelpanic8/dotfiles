{ config, makeEnable, ... }:
makeEnable config "modules.plasma" true {
  services.xserver = {
    desktopManager.plasma5.enable = true;
    displayManager.sddm.enable = true;
  };
}
