{ config, pkgs, makeEnable, inputs, ... }:
makeEnable config "myModules.hyprland" true {
  programs.hyprland = {
    enable = true;
    # Use Hyprland from the flake for proper plugin compatibility
    package = inputs.hyprland.packages.${pkgs.stdenv.hostPlatform.system}.hyprland;
    # Let UWSM manage the Hyprland session targets
    withUWSM = true;
  };

  # Hyprland-specific packages
  environment.systemPackages = with pkgs; [
    # Hyprland utilities
    kanshi         # Monitor hotplug management
    hyprpaper      # Wallpaper
    hypridle       # Idle daemon
    hyprlock       # Screen locker
    hyprcursor     # Cursor themes
    wl-clipboard   # Clipboard for Wayland
    wtype          # Wayland input typing
    cliphist       # Clipboard history
    grim           # Screenshot utility
    slurp          # Region selection
    swappy         # Screenshot annotation
    wlsunset       # Night light / blue light filter
    nwg-displays   # GUI monitor arrangement

    # hy3 plugin from flake (properly built against matching Hyprland)
    inputs.hy3.packages.${pkgs.stdenv.hostPlatform.system}.hy3

    # Hyprexpo plugin from hyprland-plugins (workspace overview)
    inputs.hyprland-plugins.packages.${pkgs.stdenv.hostPlatform.system}.hyprexpo

    # For scripts
    jq
  ];
}
