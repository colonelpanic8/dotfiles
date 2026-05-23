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
  avoidHyprlandGccIce = package:
    package.override {
      stdenv = pkgs.clangStdenv;
    };
  hyprlandGccIceOverlay = final: prev: {
    hyprland = prev.hyprland.override {
      stdenv = final.clangStdenv;
    };
    hyprland-unwrapped = final.hyprland.override {wrapRuntimeDeps = false;};
    hyprland-with-tests = final.hyprland.override {withTests = true;};
  };
  # GCC 15.2 ICEs while compiling Hyprland 0.55 on this pin. Keep the
  # Hyprland/plugin pin set intact and build Hyprland itself with Clang.
  baseHyprlandPackage = (avoidHyprlandGccIce hyprlandInput.packages.${system}.hyprland).overrideAttrs (_: {
    # Clang 21 can segfault in LLVM's Live DEBUG_VALUE analysis while compiling
    # ConfigValues.cpp when this build runs in parallel.
    enableParallelBuilding = false;
  });
  hyprlandPluginsForBase = pkgs.callPackage "${pkgs.path}/pkgs/applications/window-managers/hyprwm/hyprland-plugins" {
    hyprland = baseHyprlandPackage;
  };
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
          'nvidia_drm_device="/dev/dri/by-path/pci-0000:01:00.0-card"' \
          'intel_drm_device="/dev/dri/by-path/pci-0000:00:02.0-card"' \
          'if [ -e "$nvidia_drm_device" ] && [ -e "$intel_drm_device" ]; then' \
          '  export AQ_DRM_DEVICES="$nvidia_drm_device:$intel_drm_device"' \
          'fi' \
          'config_path="''${XDG_CONFIG_HOME:-$HOME/.config}/hypr/hyprland.lua"' \
          'exec "${package}/bin/start-hyprland" --path "${package}/bin/Hyprland" -- --config "$config_path" "$@"' \
          > "$out/bin/start-hyprland-lua"
        chmod +x "$out/bin/start-hyprland-lua"

        printf '%s\n' \
          '#!${pkgs.runtimeShell}' \
          '${cleanupStaleGraphicalSession}' \
          'exec ${pkgs.uwsm}/bin/uwsm start -g -1 -e -D Hyprland hyprland.desktop' \
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
  enableHyprglass = false;
  hyprglass = pkgs.callPackage ./packages/hyprglass {
    src = inputs.hyprglass;
    hyprland = baseHyprlandPackage;
    aquamarine = inputs.aquamarine.packages.${system}.aquamarine;
    hyprcursor = inputs.hyprcursor.packages.${system}.hyprcursor;
    hyprgraphics = inputs.hyprgraphics.packages.${system}.hyprgraphics;
    hyprlang = inputs.hyprlang.packages.${system}.hyprlang;
    hyprutils = inputs.hyprutils.packages.${system}.hyprutils;
  };
  hyprwobbly = (pkgs.callPackage "${inputs.hyprwobbly}/default.nix" {}).overrideAttrs (old: {
    patches =
      (old.patches or [])
      ++ [
        ./packages/hyprwobbly-safe-geometry-and-idle-timer.patch
      ];
  });
  hyprexpo = inputs.hyprexpo.packages.${system}.hyprexpo;
  hyprwinview = hyprlandPluginsForBase.mkHyprlandPlugin {
    pluginName = "hyprwinview";
    version = "0.1.0";
    src = inputs.hyprwinview;
    inherit (baseHyprlandPackage) nativeBuildInputs;
    buildInputs = [pkgs.librsvg];
    meta = {
      description = "A window overview plugin for Hyprland";
      homepage = "https://github.com/colonelpanic8/hyprwinview";
      license = lib.licenses.bsd3;
      platforms = lib.platforms.linux;
    };
  };
  hyprWorkspaceHistory = hyprlandPluginsForBase.mkHyprlandPlugin {
    pluginName = "hypr-workspace-history";
    version = "0.1.0";
    src = inputs.hypr-workspace-history;
    inherit (baseHyprlandPackage) nativeBuildInputs;
    meta = {
      description = "Workspace history cycling plugin for Hyprland";
      homepage = "https://github.com/colonelpanic8/hypr-workspace-history";
      license = lib.licenses.bsd3;
      platforms = lib.platforms.linux;
    };
  };
  hyprNStack = pkgs.stdenv.mkDerivation {
    pname = "hyprNStack";
    version = "0-unstable-${inputs.hyprNStack.shortRev or "dirty"}";

    src = inputs.hyprNStack;

    strictDeps = true;
    nativeBuildInputs = [
      pkgs.pkg-config
    ];
    buildInputs =
      [
        baseHyprlandPackage
      ]
      ++ baseHyprlandPackage.buildInputs;

    dontStrip = true;

    installPhase = ''
      runHook preInstall
      install -Dm755 nstackLayoutPlugin.so "$out/lib/libhyprNStack.so"
      runHook postInstall
    '';

    meta = {
      description = "N-stack layout plugin for Hyprland";
      homepage = "https://github.com/zakk4223/hyprNStack";
      license = lib.licenses.bsd3;
      platforms = baseHyprlandPackage.meta.platforms;
    };
  };
  hyprlandPluginPackages =
    [
      hyprNStack
      hyprexpo
      hyprwinview
      hyprWorkspaceHistory
      hyprwobbly
    ]
    ++ lib.optionals enableHyprglass [hyprglass];
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
    ];
    text = ''
      exec ${../dotfiles/lib/bin/hypr_shell_ui} "$@"
    '';
  };
  hyprRofiLayout = pkgs.writeShellApplication {
    name = "hypr_rofi_layout";
    runtimeInputs = [
      pkgs.coreutils
      pkgs.findutils
      pkgs.gawk
      pkgs.rofi
      hyprlandPackage
    ];
    text = ''
      exec ${../dotfiles/lib/bin/hypr_rofi_layout} "$@"
    '';
  };
  hyprRofiAction = pkgs.writeShellApplication {
    name = "hypr_rofi_action";
    runtimeInputs = [
      pkgs.libnotify
      pkgs.python3
      pkgs.rofi
      hyprlandPackage
    ];
    text = ''
      exec python3 ${../dotfiles/lib/bin/hypr_rofi_action} "$@"
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
    myModules.taffybar.enable = lib.mkDefault true;

    nixpkgs.overlays = [
      hyprlandInput.overlays.hyprland-packages
      hyprlandGccIceOverlay
    ];

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
        neowall # Shader wallpaper
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
        hyprRofiLayout
        hyprRofiAction
        hyprRofiWindow
        hyprShellUi
        jq
        lua5_4 # lua/luac for validating Hyprland Lua config
      ]
      ++ hyprlandPluginPackages;
  };
in
  enabledModule
