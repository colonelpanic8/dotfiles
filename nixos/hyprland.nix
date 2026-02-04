{ config, pkgs, lib, makeEnable, inputs, ... }:
makeEnable config "myModules.hyprland" true {
  programs.hyprland = {
    enable = true;
    # Use Hyprland from the flake for proper plugin compatibility
    package = inputs.hyprland.packages.${pkgs.stdenv.hostPlatform.system}.hyprland;
    # Let UWSM manage the Hyprland session targets
    withUWSM = true;
  };

  home-manager.sharedModules = [
    {
      programs.waybar.enable = true;

      systemd.user.services.waybar = {
        Unit = {
          Description = "Waybar";
          PartOf = [ "wayland-session@Hyprland.target" ];
          After = [ "wayland-session@Hyprland.target" ];
        };
        Service = {
          ExecStartPre = "${pkgs.bash}/bin/bash -lc 'uid=$(id -u); for i in $(seq 1 50); do runtime_dir=\"$XDG_RUNTIME_DIR\"; if [ -z \"$runtime_dir\" ]; then runtime_dir=\"/run/user/$uid\"; fi; if [ -n \"$WAYLAND_DISPLAY\" ] && [ -S \"$runtime_dir/$WAYLAND_DISPLAY\" ]; then exit 0; fi; sleep 0.1; done; exit 1'";
          ExecStart = "${pkgs.waybar}/bin/waybar";
          Restart = "always";
          RestartSec = 1;
        };
        Install = {
          WantedBy = [ "wayland-session@Hyprland.target" ];
        };
      };
    }
  ];

  # Hyprland-specific packages
  environment.systemPackages = with pkgs; [
    # Hyprland utilities
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

    # hy3 plugin from flake (properly built against matching Hyprland)
    inputs.hy3.packages.${pkgs.stdenv.hostPlatform.system}.hy3

    # Hyprexpo plugin from hyprland-plugins (workspace overview)
    inputs.hyprland-plugins.packages.${pkgs.stdenv.hostPlatform.system}.hyprexpo

    # For scripts
    jq
  ];
}
