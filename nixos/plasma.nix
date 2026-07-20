{
  config,
  lib,
  makeEnable,
  ...
}:
makeEnable config "myModules.plasma" true {
  imports = [./display-manager.nix];

  # Plasma normally uses SDDM, but the display manager is independently
  # reusable by lightweight Hyprland-only systems such as rescue media.
  myModules.displayManager.enable = lib.mkDefault true;
  services.desktopManager.plasma6.enable = true;
}
