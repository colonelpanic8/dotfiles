{ pkgs, config, makeEnable, realUsers, ... }:
makeEnable config "myModules.plasma" true {
  services.accounts-daemon.enable = true;
  services.displayManager.sddm = {
    enable = true;
    settings = {
      Users = {
        # Limit login candidates to regular interactive users.
        MaximumUid = 60000;
        MinimumUid = 1000;
        RememberLastUser = true;
      };
      Theme = {
        # Breeze hides the user chooser when users exceed this threshold.
        # Keep it aligned with declared normal users so the list stays visible.
        DisableAvatarsThreshold = (builtins.length realUsers) + 10;
      };
    };
    extraPackages = with pkgs; [
      # sddm-astronaut
    ];
  };
  services.desktopManager.plasma6.enable = true;
}
