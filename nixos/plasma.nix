{ pkgs, config, makeEnable, ... }:
makeEnable config "myModules.plasma" true {
  services.accounts-daemon.enable = true;
  services.displayManager.sddm = {
    enable = true;
    settings = {
      Users = {
        # Show a maximum number of users
        MaximumUid = 60000;
        MinimumUid = 1000;
        # Enable user icons
        RememberLastUser = true;
      };
    };
    extraPackages = with pkgs; [
      # sddm-astronaut
    ];
  };
  services.xserver = {
    desktopManager.plasma6.enable = true;
  };
}
