{
  config,
  pkgs,
  lib,
  makeEnable,
  inputs,
  ...
}:
let
  cfg = config.myModules.hyprland;
  system = pkgs.stdenv.hostPlatform.system;
  enableExternalPluginPackages = !cfg.useLuaConfigBranch;
  hyprlandInput =
    if cfg.useLuaConfigBranch
    then inputs.hyprland-lua-config
    else inputs.hyprland;
  luaPluginPackages = lib.optionals cfg.useLuaConfigBranch [
    inputs.hyprNStack.packages.${system}.hyprNStack
    inputs.hyprland-plugins-lua.packages.${system}.hyprexpo
  ];
  hyprscratchSettings = {
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
  enabledModule = makeEnable config "myModules.hyprland" true {
    myModules.taffybar.enable = true;

    # Needed for hyprlock authentication without PAM fallback warnings.
    security.pam.services.hyprlock = { };

    # DDC/CI monitor control for keyboard-driven input switching.
    hardware.i2c = {
      enable = true;
      group = "video";
    };

    programs.hyprland = {
      enable = true;
      # Keep Hyprland and plugins on a matched flake input for ABI compatibility.
      package = hyprlandInput.packages.${system}.hyprland;
      # Let UWSM manage the Hyprland session targets
      withUWSM = true;
    };

    home-manager.sharedModules = [
      inputs.hyprscratch.homeModules.default
      (
        { config, lib, ... }:
        let
          hyprConfig =
            name:
            config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/dotfiles/config/hypr/${name}";
        in
        {
          services.kanshi = {
            enable = true;
            systemdTarget = "graphical-session.target";
            settings = [
              {
                # USB-C connector names can move between DP-* ports across docks/reboots.
                # Match the ultrawide by make/model and allow the serial field to vary.
                profile.name = "ultrawide-usbc-desk";
                profile.outputs = [
                  {
                    criteria = "eDP-1";
                    status = "enable";
                    mode = "2560x1600@240Hz";
                    position = "0,0";
                    scale = 1.0;
                  }
                  {
                    criteria = "Microstep MPG341CX OLED *";
                    status = "enable";
                    mode = "3440x1440@240Hz";
                    position = "2560,0";
                    scale = 1.0;
                  }
                ];
              }
              {
                # When the laptop panel is unavailable (e.g. lid-closed docked use),
                # still drive the ultrawide at its full refresh rate.
                profile.name = "ultrawide-only";
                profile.outputs = [
                  {
                    criteria = "Microstep MPG341CX OLED *";
                    status = "enable";
                    mode = "3440x1440@240Hz";
                    position = "0,0";
                    scale = 1.0;
                  }
                ];
              }
            ];
          };

          programs.hyprscratch = {
            enable = !cfg.useLuaConfigBranch;
            settings = { };
          };

          xdg.configFile."hyprscratch/config.conf" = lib.mkIf (!cfg.useLuaConfigBranch) {
            text = lib.hm.generators.toHyprconf {
              attrs = hyprscratchSettings;
            };
          };

          xdg.configFile."hypr/hyprland.conf" = {
            force = true;
            source = hyprConfig "hyprland.conf";
          };

          xdg.configFile."hypr/hyprland.lua" = lib.mkIf cfg.useLuaConfigBranch {
            force = true;
            source = hyprConfig "hyprland.lua";
          };

          xdg.configFile."hypr/hypridle.conf".source = hyprConfig "hypridle.conf";

          xdg.configFile."hypr/hyprlock.conf".source = hyprConfig "hyprlock.conf";

          xdg.configFile."hypr/scripts".enable = false;
        }
      )
    ];

    # Hyprland-specific packages
    environment.systemPackages =
      with pkgs;
      [
        # Hyprland utilities
        hyprpaper # Wallpaper
        hypridle # Idle daemon
        hyprlock # Screen locker
        hyprcursor # Cursor themes
        wl-clipboard # Clipboard for Wayland
        wtype # Wayland input typing
        cliphist # Clipboard history
        grim # Screenshot utility
        slurp # Region selection
        swappy # Screenshot annotation
        nwg-displays # GUI monitor arrangement
        mpvpaper # Layer-shell video screensaver payload
        ddcutil # Monitor input switching over DDC/CI

        # For scripts
        jq
      ]
      ++ luaPluginPackages
      ++ lib.optionals enableExternalPluginPackages [
        # External plugin packages are pinned to the stable 0.53 stack.
        # Keep hy3 on the stable stack; the Lua branch uses hyprNStack and the
        # forked Lua-compatible hyprexpo input instead.
        inputs.hy3.packages.${system}.hy3
        inputs.hyprland-plugins.packages.${system}.hyprexpo
      ];
  };
in
enabledModule
// {
  options = lib.recursiveUpdate enabledModule.options {
    myModules.hyprland.useLuaConfigBranch = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = ''
        Use the experimental Hyprland PR 13817 Lua-config branch for the
        Hyprland package itself. The experimental package set excludes hy3, and
        includes the Lua-branch builds of hyprNStack and hyprexpo instead. When
        a sibling `hyprland.lua` is present, the Lua config manager picks it
        before `hyprland.conf`.
      '';
    };
  };
}
