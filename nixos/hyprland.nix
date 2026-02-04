{ config, pkgs, lib, makeEnable, inputs, ... }:
makeEnable config "myModules.hyprland" true {
  programs.hyprland = {
    enable = true;
    # Use Hyprland from the flake for proper plugin compatibility
    package = inputs.hyprland.packages.${pkgs.stdenv.hostPlatform.system}.hyprland;
  };

  home-manager.sharedModules = [
    {
      xdg.configFile."waybar" = {
        source = ../dotfiles/config/waybar;
        recursive = true;
        force = true;
      };

      programs.waybar.enable = true;

      systemd.user.targets.hyprland-session = {
        Unit = {
          Description = "Hyprland session";
        };
      };

      systemd.user.services.waybar = {
        Unit = {
          Description = "Waybar";
          PartOf = [ "hyprland-session.target" ];
          After = [ "hyprland-session.target" ];
        };
        Service = {
          ExecStartPre = "${pkgs.bash}/bin/bash -lc 'uid=$(id -u); for i in $(seq 1 50); do runtime_dir=\"$XDG_RUNTIME_DIR\"; if [ -z \"$runtime_dir\" ]; then runtime_dir=\"/run/user/$uid\"; fi; if [ -n \"$WAYLAND_DISPLAY\" ] && [ -S \"$runtime_dir/$WAYLAND_DISPLAY\" ]; then exit 0; fi; sleep 0.1; done; exit 1'";
          ExecStart = "${pkgs.waybar}/bin/waybar";
          Restart = "always";
          RestartSec = 1;
        };
        Install = {
          WantedBy = [ "hyprland-session.target" ];
        };
      };

      programs.hyprpanel.enable = false;
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
