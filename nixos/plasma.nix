{ pkgs, config, makeEnable, ... }:
makeEnable config "myModules.plasma" true {
  services.displayManager.sddm = {
    enable = true;
    extraPackages = with pkgs; [
      # sddm-astronaut
    ];
  };
  services.xserver = {
    desktopManager.plasma5.enable = true;
  };
}
