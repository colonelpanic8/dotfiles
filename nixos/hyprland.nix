{ config, pkgs, lib, makeEnable, ... }:
makeEnable config "myModules.hyprland" true {
  programs.hyprland = {
    enable = true;
    # Plugins for XMonad-like experience
    plugins = [
      pkgs.hyprlandPlugins.hy3        # Dynamic tiling like XMonad
      pkgs.hyprlandPlugins.hyprexpo   # Expose/overview (like skippy-xd)
    ];
  };

  # Hyprland-specific packages
  environment.systemPackages = with pkgs; [
    # Hyprland utilities
    hyprpaper      # Wallpaper
    hypridle       # Idle daemon
    hyprlock       # Screen locker
    hyprcursor     # Cursor themes
    wl-clipboard   # Clipboard for Wayland
    cliphist       # Clipboard history
    grim           # Screenshot utility
    slurp          # Region selection
    swappy         # Screenshot annotation
    wlsunset       # Night light / blue light filter

    # For hy3 directional focus
    jq             # JSON processing (used in scripts)
  ];
}
