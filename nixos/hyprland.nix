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
  hyprlandInputPkgs = import hyprlandInput.inputs.nixpkgs {inherit system;};
  makeHyprlandGlaze = glaze:
    (glaze.override {
      enableSIMD = false;
    }).overrideAttrs (old: {
      cmakeFlags =
        (old.cmakeFlags or [])
        ++ [
          "-Dglaze_DEVELOPER_MODE=OFF"
          "-Dglaze_ENABLE_FUZZING=OFF"
          "-DBUILD_TESTING=OFF"
        ];
    });
  avoidHyprlandGccIce = glaze: package:
    (package.override {
      stdenv = pkgs.clangStdenv;
    }).overrideAttrs (old: {
      buildInputs =
        map
        (input:
          if (input.pname or null) == "glaze"
          then makeHyprlandGlaze glaze
          else input)
        (old.buildInputs or []);
    });
  avoidOverlayHyprlandGccIce = final: package:
    (package.override {
      stdenv = final.clangStdenv;
    }).overrideAttrs (old: {
      buildInputs =
        map
        (input:
          if (input.pname or null) == "glaze"
          then makeHyprlandGlaze final.glaze
          else input)
        (old.buildInputs or []);
    });
  hyprlandGccIceOverlay = final: prev: {
    glaze = makeHyprlandGlaze prev.glaze;
    hyprland = avoidOverlayHyprlandGccIce final prev.hyprland;
    hyprland-unwrapped = final.hyprland.override {wrapRuntimeDeps = false;};
    hyprland-with-tests = final.hyprland.override {withTests = true;};
  };
  # GCC 15.2 ICEs while compiling Hyprland 0.55 on this pin. Keep the
  # Hyprland/plugin pin set intact and build Hyprland itself with Clang.
  baseHyprlandPackage = (avoidHyprlandGccIce hyprlandInputPkgs.glaze hyprlandInput.packages.${system}.hyprland).overrideAttrs (_: {
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
      meta =
        builtins.removeAttrs (package.meta or {}) ["outputsToInstall"]
        // {
          mainProgram = package.meta.mainProgram or "Hyprland";
        };
      passthru =
        (package.passthru or {})
        // {
          providedSessions = package.passthru.providedSessions or ["hyprland"];
        };
      postBuild = ''
        mkdir -p "$out/bin" "$out/share/wayland-sessions"

        rm -f "$out/bin/Hyprland"
        printf '%s\n' \
          '#!${pkgs.runtimeShell}' \
          'exec ${pkgs.runtimeShell} "${package}/bin/Hyprland" "$@"' \
          > "$out/bin/Hyprland"
        chmod +x "$out/bin/Hyprland"

        printf '%s\n' \
          '#!${pkgs.runtimeShell}' \
          'nvidia_drm_device="/dev/dri/by-path/pci-0000:01:00.0-card"' \
          'intel_drm_device="/dev/dri/by-path/pci-0000:00:02.0-card"' \
          'if [ -e "$nvidia_drm_device" ] && [ -e "$intel_drm_device" ]; then' \
          '  # AQ_DRM_DEVICES is colon-delimited, so PCI by-path names cannot be used directly.' \
          '  nvidia_drm_device="$(${pkgs.coreutils}/bin/readlink -f "$nvidia_drm_device")"' \
          '  intel_drm_device="$(${pkgs.coreutils}/bin/readlink -f "$intel_drm_device")"' \
          '  mux_mode="$(${pkgs.coreutils}/bin/cat /sys/devices/platform/asus-nb-wmi/gpu_mux_mode 2>/dev/null || true)"' \
          '  if [ "$mux_mode" = 1 ]; then' \
          '    # Optimus/Hybrid: render on Intel and retain NVIDIA for compute/offload.' \
          '    export AQ_DRM_DEVICES="$intel_drm_device:$nvidia_drm_device"' \
          '  else' \
          '    # Hardware dGPU MUX, or an unknown platform: preserve NVIDIA-first behavior.' \
          '    export AQ_DRM_DEVICES="$nvidia_drm_device:$intel_drm_device"' \
          '  fi' \
          'fi' \
          'config_path="''${XDG_CONFIG_HOME:-$HOME/.config}/hypr/hyprland.lua"' \
          'exec "${package}/bin/start-hyprland" --path "@out@/bin/Hyprland" -- --config "$config_path" "$@"' \
          > "$out/bin/start-hyprland-lua"
        substituteInPlace "$out/bin/start-hyprland-lua" --replace-fail "@out@" "$out"
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
  hyprlandPortalPackage = config.programs.hyprland.portalPackage;
  hyprlandGapsEnabledString =
    if config.myModules.hyprland.gaps.enable
    then "1"
    else "0";
  hyprlandCursorSizeString = toString config.myModules.hyprland.cursorSize;
  voxtypePackage = inputs.voxtype.packages.${system}.vulkan;
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
  hyprtasking = pkgs.gcc14Stdenv.mkDerivation {
    pname = "hyprtasking";
    version = "0.1.0-unstable-${inputs.hyprtasking.shortRev or "dirty"}";
    src = inputs.hyprtasking;

    strictDeps = true;
    nativeBuildInputs =
      [
        pkgs.pkg-config
        pkgs.meson
        pkgs.ninja
      ]
      ++ baseHyprlandPackage.nativeBuildInputs;
    buildInputs = [baseHyprlandPackage] ++ baseHyprlandPackage.buildInputs;

    meta = {
      description = "Powerful workspace management plugin for Hyprland";
      homepage = "https://github.com/raybbian/hyprtasking";
      license = lib.licenses.bsd3;
      platforms = lib.platforms.linux;
    };
  };
  hyprDynamicCursors = pkgs.gcc14Stdenv.mkDerivation {
    pname = "hypr-dynamic-cursors";
    version = "0-unstable-${inputs.hypr-dynamic-cursors.shortRev or "dirty"}";
    src = inputs.hypr-dynamic-cursors;

    strictDeps = true;
    nativeBuildInputs =
      baseHyprlandPackage.nativeBuildInputs
      ++ [
        baseHyprlandPackage
        pkgs.pkg-config
      ];
    buildInputs = [(lib.getDev baseHyprlandPackage)] ++ baseHyprlandPackage.buildInputs;

    dontConfigure = true;
    dontUseCmakeConfigure = true;
    dontUseMesonConfigure = true;
    dontUseNinjaBuild = true;
    dontUseNinjaInstall = true;

    buildPhase = ''
      runHook preBuild
      make
      runHook postBuild
    '';

    installPhase = ''
      runHook preInstall
      install -Dm755 out/dynamic-cursors.so "$out/lib/libhypr-dynamic-cursors.so"
      runHook postInstall
    '';

    meta = {
      description = "A plugin to make Hyprland cursor movement more realistic";
      homepage = "https://github.com/VirtCode/hypr-dynamic-cursors";
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
      hyprwinview
      hyprWorkspaceHistory
      hyprtasking
      hyprDynamicCursors
    ]
    ++ lib.optionals enableHyprglass [hyprglass];
  llvmNmCompat = pkgs.writeShellApplication {
    name = "llvm-nm";
    runtimeInputs = [pkgs.binutils];
    text = ''
      exec nm "$@"
    '';
  };
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

    services.rumno.enable = true;

    environment.sessionVariables = {
      IMALISON_HYPRLAND_GAPS = hyprlandGapsEnabledString;
      IMALISON_HYPRLAND_CURSOR_SIZE = hyprlandCursorSizeString;
      XCURSOR_SIZE = hyprlandCursorSizeString;
      HYPRCURSOR_SIZE = hyprlandCursorSizeString;
    };

    home-manager.users.imalison = lib.mkIf config.myModules.voxtype.enable {
      imports = [inputs.voxtype.homeManagerModules.default];

      home.packages = [inputs.voxtype.packages.${system}.osd-gtk4];

      programs.voxtype = {
        enable = true;
        package = voxtypePackage;
        engine = "whisper";
        model.name = "large-v3-turbo";
        service.enable = true;

        settings = {
          hotkey.enabled = false;

          audio = {
            device = "default";
            max_duration_secs = 120;
          };

          whisper = {
            language = "en";
            translate = false;
            on_demand_loading = false;
            initial_prompt = "Ivan Malison, Railbird, NixOS, Hyprland, Taffybar, Emacs, Codex.";
          };

          output = {
            mode = "type";
            fallback_to_clipboard = true;
            pre_type_delay_ms = 100;
            notification = {
              on_recording_start = true;
              on_recording_stop = true;
              on_transcription = false;
            };
          };
        };
      };
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

          home.sessionVariables = {
            IMALISON_HYPRLAND_GAPS = hyprlandGapsEnabledString;
            IMALISON_HYPRLAND_CURSOR_SIZE = hyprlandCursorSizeString;
            XCURSOR_SIZE = hyprlandCursorSizeString;
            HYPRCURSOR_SIZE = hyprlandCursorSizeString;
          };
          systemd.user.sessionVariables = {
            IMALISON_HYPRLAND_GAPS = hyprlandGapsEnabledString;
            IMALISON_HYPRLAND_CURSOR_SIZE = hyprlandCursorSizeString;
            XCURSOR_SIZE = hyprlandCursorSizeString;
            HYPRCURSOR_SIZE = hyprlandCursorSizeString;
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

          # File chooser requests from early-started Electron apps go through
          # xdg-desktop-portal-gtk. The upstream portal backend units wait for
          # graphical-session.target, which UWSM can reach well after Wayland
          # and D-Bus are already usable. That makes D-Bus activation time out
          # and can abort the requesting app.
          xdg.configFile."systemd/user/xdg-desktop-portal-gtk.service".text = ''
            [Unit]
            Description=Portal service (GTK/GNOME implementation)
            PartOf=graphical-session.target

            [Service]
            Type=dbus
            BusName=org.freedesktop.impl.portal.desktop.gtk
            ExecStart=${pkgs.xdg-desktop-portal-gtk}/libexec/xdg-desktop-portal-gtk
          '';

          xdg.configFile."systemd/user/xdg-desktop-portal-hyprland.service".text = ''
            [Unit]
            Description=Portal service (Hyprland implementation)
            PartOf=graphical-session.target
            ConditionEnvironment=WAYLAND_DISPLAY

            [Service]
            Type=dbus
            BusName=org.freedesktop.impl.portal.desktop.hyprland
            ExecStart=${hyprlandPortalPackage}/libexec/xdg-desktop-portal-hyprland
            Restart=on-failure
            Slice=session.slice
          '';

          # The GTK file-chooser's right-click context menu (Show Hidden Files,
          # etc.) is unreliable under Hyprland/wlroots (upstream popup-focus bug,
          # hyprwm/Hyprland#6426). Route only the FileChooser interface to the
          # KDE (Qt) portal backend, whose dialogs don't have that bug. Screencast/
          # screenshot stay on hyprland; everything else falls back to gtk. The KDE
          # backend (xdg-desktop-portal-kde) is already present via plasma6.
          xdg.configFile."xdg-desktop-portal/hyprland-portals.conf".text = ''
            [preferred]
            default=hyprland;gtk
            org.freedesktop.impl.portal.FileChooser=kde
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
        llvmNmCompat # Hyprland's Clang-built plugin API shells out to llvm-nm

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
