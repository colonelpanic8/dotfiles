{ config, pkgs, makeEnable, inputs, ... }:
let
  system = pkgs.stdenv.hostPlatform.system;
  hyprexpoPatched = inputs.hyprland-plugins.packages.${system}.hyprexpo.overrideAttrs (old: {
    patches = (old.patches or [ ]) ++ [
      ./patches/hyprexpo-pr-612-workspace-numbers.patch
      ./patches/hyprexpo-pr-616-bring-mode.patch
    ];
  });
in
makeEnable config "myModules.hyprland" true {
  myModules.taffybar.enable = true;

  programs.hyprland = {
    enable = true;
    # Use Hyprland from the flake for proper plugin compatibility
    package = inputs.hyprland.packages.${pkgs.stdenv.hostPlatform.system}.hyprland;
    # Let UWSM manage the Hyprland session targets
    withUWSM = true;
  };

  home-manager.sharedModules = [
    inputs.hyprscratch.homeModules.default
    {
      services.kanshi = {
        enable = true;
        systemdTarget = "hyprland-session.target";
      };

      programs.hyprscratch = {
        enable = true;
        settings = {
          daemon_options = "clean";
          global_options = "";
          global_rules = "float;size monitor_w*0.95 monitor_h*0.95;center";

          htop = {
            command = "alacritty --class htop-scratch --title htop -e htop";
            class = "htop-scratch";
          };

          volume = {
            command = "pavucontrol";
            class = "org.pulseaudio.pavucontrol";
          };

          spotify = {
            command = "spotify";
            class = "spotify";
          };

          element = {
            command = "element-desktop";
            class = "Element";
          };

          slack = {
            command = "slack";
            class = "Slack";
          };

          transmission = {
            command = "transmission-gtk";
            class = "transmission-gtk";
          };

          dropdown = {
            command = "ghostty --config-file=/home/imalison/.config/ghostty/dropdown";
            class = "com.mitchellh.ghostty.dropdown";
            options = "persist";
            rules = "float;size monitor_w monitor_h*0.5;move 0 60;noborder;noshadow;animation slide";
          };

          gmail = {
            command = "google-chrome-stable --new-window https://mail.google.com/mail/u/0/#inbox";
            class = "google-chrome";
            title = "Gmail";
          };

          messages = {
            command = "google-chrome-stable --new-window https://messages.google.com/web/conversations";
            class = "google-chrome";
            title = "Messages";
          };
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
    nwg-displays   # GUI monitor arrangement

    # hy3 plugin from flake (properly built against matching Hyprland)
    inputs.hy3.packages.${pkgs.stdenv.hostPlatform.system}.hy3

    # Hyprexpo plugin from hyprland-plugins (workspace overview)
    hyprexpoPatched

    # For scripts
    jq
  ];
}
