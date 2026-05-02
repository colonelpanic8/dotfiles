{
  config,
  pkgs,
  lib,
  makeEnable,
  inputs,
  ...
}: let
  system = pkgs.stdenv.hostPlatform.system;
  hyprlandInput = inputs.hyprland;
  baseHyprlandPackage = hyprlandInput.packages.${system}.hyprland;
  cleanupStaleGraphicalSession = pkgs.writeShellScript "cleanup-stale-graphical-session" ''
    set -u

    # Only clean targets that are plainly stale. If a compositor is still
    # running, let the active session own its own shutdown path.
    if ${pkgs.procps}/bin/pgrep -u "$(${pkgs.coreutils}/bin/id -u)" -f '(^|/)(Hyprland|\.Hyprland-wrapped|river|kwin_wayland)( |$)' >/dev/null 2>&1; then
      exit 0
    fi

    ${pkgs.systemd}/bin/systemctl --user stop \
      hyprland-session.target \
      river-xmonad-session.target \
      graphical-session.target \
      graphical-session-pre.target \
      tray.target \
      2>/dev/null || true

    ${pkgs.systemd}/bin/systemctl --user unset-environment \
      WAYLAND_DISPLAY \
      DISPLAY \
      XAUTHORITY \
      HYPRLAND_INSTANCE_SIGNATURE \
      XDG_CURRENT_DESKTOP \
      XDG_SESSION_DESKTOP \
      XDG_SESSION_TYPE \
      IMALISON_SESSION_TYPE \
      IMALISON_WINDOW_MANAGER \
      2>/dev/null || true

    ${pkgs.systemd}/bin/systemctl --user reset-failed 2>/dev/null || true
  '';
  makeHyprlandLuaPackage = package:
    (pkgs.symlinkJoin {
      name = "${package.name}-lua-config";
      inherit (package) version;
      paths = [package];
      passthru =
        (package.passthru or {})
        // {
          providedSessions = package.passthru.providedSessions or ["hyprland"];
        };
      postBuild = ''
        mkdir -p "$out/bin" "$out/share/wayland-sessions"
        printf '%s\n' \
          '#!${pkgs.runtimeShell}' \
          'config_path="''${XDG_CONFIG_HOME:-$HOME/.config}/hypr/hyprland.lua"' \
          'exec "${package}/bin/Hyprland" --config "$config_path" "$@"' \
          > "$out/bin/start-hyprland-lua"
        chmod +x "$out/bin/start-hyprland-lua"

        printf '%s\n' \
          '#!${pkgs.runtimeShell}' \
          '${cleanupStaleGraphicalSession}' \
          'exec ${pkgs.uwsm}/bin/uwsm start -e -D Hyprland hyprland.desktop' \
          > "$out/bin/start-hyprland-uwsm-clean"
        chmod +x "$out/bin/start-hyprland-uwsm-clean"

        rm -f "$out/share/wayland-sessions/hyprland.desktop"
        substitute \
          "${package}/share/wayland-sessions/hyprland.desktop" \
          "$out/share/wayland-sessions/hyprland.desktop" \
          --replace-fail \
          "Exec=${package}/bin/start-hyprland" \
          "Exec=$out/bin/start-hyprland-lua"

        rm -f "$out/share/wayland-sessions/hyprland-uwsm.desktop"
        substitute \
          "${package}/share/wayland-sessions/hyprland-uwsm.desktop" \
          "$out/share/wayland-sessions/hyprland-uwsm.desktop" \
          --replace-fail \
          "Exec=uwsm start -e -D Hyprland hyprland.desktop" \
          "Exec=$out/bin/start-hyprland-uwsm-clean"
      '';
    })
    // {
      override = {enableXWayland ? true, ...} @ args:
        makeHyprlandLuaPackage (package.override args);
      overrideAttrs = f: makeHyprlandLuaPackage (package.overrideAttrs f);
    };
  hyprlandPackage = makeHyprlandLuaPackage baseHyprlandPackage;
  hyprlandPluginPackages = [
    inputs.hyprNStack.packages.${system}.hyprNStack
    inputs.hyprland-plugins-lua.packages.${system}.hyprexpo
    inputs.hyprwinview.packages.${system}.hyprwinview
    inputs.hypr-workspace-history.packages.${system}.hypr-workspace-history
  ];
  hyprRofiWindow = pkgs.writeShellApplication {
    name = "hypr_rofi_window";
    runtimeInputs = [
      pkgs.python3
      pkgs.rofi
      hyprlandPackage
    ];
    text = ''
      exec python3 ${../dotfiles/lib/bin/hypr_rofi_window} "$@"
    '';
  };
  hyprShellUi = pkgs.writeShellApplication {
    name = "hypr_shell_ui";
    runtimeInputs = [
      pkgs.rofi
      hyprRofiWindow
      inputs.noctalia.packages.${system}.default
    ];
    text = ''
      exec ${../dotfiles/lib/bin/hypr_shell_ui} "$@"
    '';
  };
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
  };
  enabledModule = makeEnable config "myModules.hyprland" true {
    # Install both shell service units so `desktop_shell_ui set ...` can switch
    # between them at runtime without a NixOS rebuild.
    myModules.noctalia.enable = lib.mkDefault true;
    myModules.taffybar.enable = lib.mkDefault true;

    # Needed for hyprlock authentication without PAM fallback warnings.
    security.pam.services.hyprlock = {};

    # DDC/CI monitor control for keyboard-driven input switching.
    hardware.i2c = {
      enable = true;
      group = "video";
    };

    programs.hyprland = {
      enable = true;
      # Keep Hyprland and plugins on a matched flake input for ABI compatibility.
      package = hyprlandPackage;
      # Let UWSM manage the Hyprland session targets
      withUWSM = true;
    };

    home-manager.sharedModules = [
      inputs.hyprscratch.homeModules.default
      (
        {
          config,
          lib,
          ...
        }: {
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
            enable = false;
            settings = {};
          };

          xdg.configFile."hyprscratch/config.conf" = lib.mkIf false {
            text = lib.hm.generators.toHyprconf {
              attrs = hyprscratchSettings;
            };
          };

          xdg.configFile."systemd/user/wayland-wm@hyprland.desktop.service.d/10-cleanup-stale-session.conf".text = ''
            [Service]
            ExecStopPost=${cleanupStaleGraphicalSession}
          '';
        }
      )
    ];

    # Hyprland-specific packages
    environment.systemPackages = with pkgs;
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
        hyprRofiWindow
        hyprShellUi
        jq
      ]
      ++ hyprlandPluginPackages;
  };
in
  enabledModule
