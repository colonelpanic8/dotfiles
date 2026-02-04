{ config, pkgs, lib, makeEnable, inputs, ... }:
makeEnable config "myModules.hyprland" true {
  programs.hyprland = {
    enable = true;
    # Use Hyprland from the flake for proper plugin compatibility
    package = inputs.hyprland.packages.${pkgs.stdenv.hostPlatform.system}.hyprland;
  };

  home-manager.sharedModules = [
    {
      # Wire the Hyprland config from dotfiles into ~/.config/hypr
      xdg.configFile."hypr" = {
        source = ../dotfiles/config/hypr;
        recursive = true;
        force = true;
      };

      programs.waybar = {
        enable = true;
        systemd.enable = true;
        settings = [
          {
            layer = "top";
            position = "top";
            height = 30;
            "modules-left" = [ "hyprland/workspaces" ];
            "modules-center" = [ "hyprland/window" ];
            "modules-right" = [ "tray" "clock" ];

            "hyprland/workspaces" = {
              format = "{id}:{windows}";
              "format-window-separator" = " ";
              "window-rewrite-default" = "";
              "window-rewrite" = {
                "class<firefox>" = "";
                "class<google-chrome|chromium>" = "";
                "class<code|codium>" = "";
                "class<Alacritty|kitty|foot>" = "";
                "class<Slack>" = "";
                "class<Spotify>" = "";
                "class<Element|discord>" = "";
                "class<pavucontrol>" = "";
                "class<transmission-gtk>" = "";
              };
              "persistent-workspaces" = { "*" = 10; };
              "all-outputs" = true;
              "on-click" = "activate";
            };

            "hyprland/window" = {
              format = "{class}: {title}";
              "separate-outputs" = true;
            };

            tray = {
              spacing = 8;
            };

            clock = {
              format = "{:%a %b %d %I:%M:%S %p}";
            };
          }
        ];
        style = ''
        * {
          border: none;
          border-radius: 0;
          font-family: "Roboto", "JetBrainsMono Nerd Font";
          font-size: 11pt;
          min-height: 0;
        }

        window#waybar {
          background: rgba(24, 24, 24, 0.92);
          color: #e6e6e6;
        }

        #workspaces {
          margin-left: 6px;
        }

        #workspaces button {
          padding: 0 8px;
          margin: 4px 4px;
          border-radius: 6px;
          background: transparent;
          color: #bfbfbf;
        }

        #workspaces button.active {
          background: #e0b45a;
          color: #1c1c1c;
        }

        #workspaces button.visible {
          background: #3a3a3a;
          color: #e6e6e6;
        }

        #workspaces button.urgent {
          background: #e06060;
          color: #1c1c1c;
        }

        #workspaces button.empty {
          color: #777777;
        }

        #window {
          padding: 0 10px;
        }

        #tray,
        #clock {
          padding: 0 10px;
        }
        '';
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
    cliphist       # Clipboard history
    grim           # Screenshot utility
    slurp          # Region selection
    swappy         # Screenshot annotation
    wlsunset       # Night light / blue light filter

    # hy3 plugin from flake (properly built against matching Hyprland)
    inputs.hy3.packages.${pkgs.stdenv.hostPlatform.system}.hy3

    # For scripts
    jq
  ];
}
