{
  config,
  pkgs,
  lib,
  makeEnable,
  inputs,
  ...
}: let
  cfg = config.myModules.hyprland;
  session = import ./session-variables.nix;
  system = pkgs.stdenv.hostPlatform.system;
  hyprlandInput = inputs.hyprland;
  # GCC 15 ICEs while compiling Hyprland 0.55's ConfigManager. GCC 16 builds
  # the unmodified source, avoiding the old Clang-only source compatibility patch.
  baseHyprlandPackage = hyprlandInput.packages.${system}.hyprland.override {
    stdenv = pkgs.gcc16Stdenv;
  };
  hyprlandGcc16Overlay = final: prev: {
    hyprland = prev.hyprland.override {
      stdenv = final.gcc16Stdenv;
    };
    hyprland-unwrapped = final.hyprland.override {wrapRuntimeDeps = false;};
    hyprland-with-tests = final.hyprland.override {withTests = true;};
  };
  hyprlandPluginsForBase = pkgs.callPackage "${pkgs.path}/pkgs/applications/window-managers/hyprwm/hyprland-plugins" {
    hyprland = baseHyprlandPackage;
  };
  cleanupStaleGraphicalSession = pkgs.writeShellScript "cleanup-stale-graphical-session" ''
    set -u

    # Do not tear down another live graphical login. A compositor can outlive
    # its SDDM session when the display manager is restarted, though; in that
    # case it is stale and must not keep UWSM from starting the new session.
    if ${pkgs.procps}/bin/pgrep -u "$(${pkgs.coreutils}/bin/id -u)" -f '(^|/)(Hyprland|\.Hyprland-wrapped|river|kwin_wayland)( |$)' >/dev/null 2>&1; then
      current_session="''${XDG_SESSION_ID:-}"
      user_id="$(${pkgs.coreutils}/bin/id -u)"

      while read -r session_id session_uid _; do
        if [[ "$session_uid" != "$user_id" || "$session_id" == "$current_session" ]]; then
          continue
        fi

        session_type="$(${pkgs.systemd}/bin/loginctl show-session "$session_id" --property=Type --value 2>/dev/null || true)"
        session_state="$(${pkgs.systemd}/bin/loginctl show-session "$session_id" --property=State --value 2>/dev/null || true)"
        case "$session_type:$session_state" in
          wayland:active|wayland:online|x11:active|x11:online)
            exit 0
            ;;
        esac
      done < <(${pkgs.systemd}/bin/loginctl list-sessions --no-legend 2>/dev/null)
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
          'debug_log="''${XDG_RUNTIME_DIR:-/tmp}/hyprland-start.log"' \
          'log_start_context() {' \
          '  {' \
          '    printf "=== %s start-hyprland-lua ===\n" "$(${pkgs.coreutils}/bin/date --iso-8601=seconds 2>/dev/null || ${pkgs.coreutils}/bin/date)"' \
          '    printf "argv=%s\n" "$*"' \
          '    printf "wrapper=%s\n" "$0"' \
          '    printf "uid=%s runtime=%s session=%s vt=%s\n" "$(${pkgs.coreutils}/bin/id -u 2>/dev/null || true)" "''${XDG_RUNTIME_DIR:-}" "''${XDG_SESSION_ID:-}" "''${XDG_VTNR:-}"' \
          '    printf "desktop=%s session_desktop=%s session_type=%s\n" "''${XDG_CURRENT_DESKTOP:-}" "''${XDG_SESSION_DESKTOP:-}" "''${XDG_SESSION_TYPE:-}"' \
          '    printf "display=%s wayland=%s hypr_sig=%s\n" "''${DISPLAY:-}" "''${WAYLAND_DISPLAY:-}" "''${HYPRLAND_INSTANCE_SIGNATURE:-}"' \
          '  } >> "$debug_log" 2>&1 || true' \
          '}' \
          'log_start_context "$@"' \
          'if [ -z "''${AQ_DRM_DEVICES:-}" ]; then' \
          '  append_drm_device() {' \
          '    if [ -z "''${AQ_DRM_DEVICES:-}" ]; then' \
          '      AQ_DRM_DEVICES="$1"' \
          '    else' \
          '      AQ_DRM_DEVICES="$AQ_DRM_DEVICES:$1"' \
          '    fi' \
          '  }' \
          '  nvidia_drm_device=""' \
          '  intel_drm_device=""' \
          '  all_drm_devices=""' \
          '  for drm_path in /dev/dri/by-path/*-card; do' \
          '    [ -e "$drm_path" ] || continue' \
          '    case "$drm_path" in *platform-simple-framebuffer*) continue ;; esac' \
          '    resolved_drm_device="$(${pkgs.coreutils}/bin/readlink -f "$drm_path")"' \
          '    card_name="$(${pkgs.coreutils}/bin/basename "$resolved_drm_device")"' \
          '    vendor="$(${pkgs.coreutils}/bin/cat "/sys/class/drm/$card_name/device/vendor" 2>/dev/null || true)"' \
          '    if [ -z "$all_drm_devices" ]; then' \
          '      all_drm_devices="$resolved_drm_device"' \
          '    else' \
          '      all_drm_devices="$all_drm_devices:$resolved_drm_device"' \
          '    fi' \
          '    case "$vendor" in' \
          '      0x10de) nvidia_drm_device="$resolved_drm_device" ;;' \
          '      0x8086) intel_drm_device="$resolved_drm_device" ;;' \
          '    esac' \
          '  done' \
          '  if [ -n "$nvidia_drm_device" ] && [ -n "$intel_drm_device" ]; then' \
          '  mux_mode="$(${pkgs.coreutils}/bin/cat /sys/devices/platform/asus-nb-wmi/gpu_mux_mode 2>/dev/null || true)"' \
          '    if [ "$mux_mode" = 1 ]; then' \
          '      # Optimus/Hybrid: render on Intel and retain NVIDIA for compute/offload.' \
          '      append_drm_device "$intel_drm_device"' \
          '      append_drm_device "$nvidia_drm_device"' \
          '    else' \
          '      # Hardware dGPU MUX, or an unknown platform: preserve NVIDIA-first behavior.' \
          '      append_drm_device "$nvidia_drm_device"' \
          '      append_drm_device "$intel_drm_device"' \
          '    fi' \
          '  elif [ -n "$nvidia_drm_device" ]; then' \
          '    append_drm_device "$nvidia_drm_device"' \
          '  elif [ -n "$intel_drm_device" ]; then' \
          '    append_drm_device "$intel_drm_device"' \
          '  elif [ -n "$all_drm_devices" ]; then' \
          '    AQ_DRM_DEVICES="$all_drm_devices"' \
          '  fi' \
          '  export AQ_DRM_DEVICES' \
          'fi' \
          'printf "AQ_DRM_DEVICES=%s\n" "''${AQ_DRM_DEVICES:-}" >> "$debug_log" 2>&1 || true' \
          'config_path="''${XDG_CONFIG_HOME:-$HOME/.config}/hypr/hyprland.lua"' \
          'printf "config_path=%s\nexec=%s --path %s -- --config %s %s\n" "$config_path" "${package}/bin/start-hyprland" "@out@/bin/Hyprland" "$config_path" "$*" >> "$debug_log" 2>&1 || true' \
          'exec "${package}/bin/start-hyprland" --path "@out@/bin/Hyprland" -- --config "$config_path" "$@"' \
          > "$out/bin/start-hyprland-lua"
        substituteInPlace "$out/bin/start-hyprland-lua" --replace-fail "@out@" "$out"
        chmod +x "$out/bin/start-hyprland-lua"

        printf '%s\n' \
          '#!${pkgs.runtimeShell}' \
          'debug_log="''${XDG_RUNTIME_DIR:-/tmp}/hyprland-start.log"' \
          '{' \
          '  printf "=== %s start-hyprland-uwsm-clean ===\n" "$(${pkgs.coreutils}/bin/date --iso-8601=seconds 2>/dev/null || ${pkgs.coreutils}/bin/date)"' \
          '  printf "argv=%s wrapper=%s uid=%s runtime=%s session=%s vt=%s\n" "$*" "$0" "$(${pkgs.coreutils}/bin/id -u 2>/dev/null || true)" "''${XDG_RUNTIME_DIR:-}" "''${XDG_SESSION_ID:-}" "''${XDG_VTNR:-}"' \
          '  printf "desktop=%s session_desktop=%s session_type=%s display=%s wayland=%s\n" "''${XDG_CURRENT_DESKTOP:-}" "''${XDG_SESSION_DESKTOP:-}" "''${XDG_SESSION_TYPE:-}" "''${DISPLAY:-}" "''${WAYLAND_DISPLAY:-}"' \
          '} >> "$debug_log" 2>&1 || true' \
          '${cleanupStaleGraphicalSession}' \
          'printf "exec=%s start -g -1 -e -D Hyprland hyprland.desktop\n" "${pkgs.uwsm}/bin/uwsm" >> "$debug_log" 2>&1 || true' \
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
  hyprsaver = pkgs.callPackage ./packages/hyprsaver {
    src = inputs.hyprsaver;
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
      hyprlandGcc16Overlay
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

    services.rumno.enable = !cfg.portable;

    environment.sessionVariables = {
      IMALISON_HYPRLAND_GAPS = hyprlandGapsEnabledString;
      IMALISON_HYPRLAND_CURSOR_SIZE = hyprlandCursorSizeString;
      XCURSOR_SIZE = hyprlandCursorSizeString;
      HYPRCURSOR_SIZE = hyprlandCursorSizeString;
    };

    home-manager.sharedModules = [
      inputs.hyprscratch.homeModules.default
      (
        {
          config,
          lib,
          ...
        }: {
          systemd.user.targets.hyprland-session = {
            Unit = {
              Description = "Hyprland session (custom)";
              ConditionEnvironment = session.hyprland;
            };
          };

          services.kanshi = lib.mkIf (!cfg.portable) {
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
            ${lib.optionalString (!cfg.portable) "org.freedesktop.impl.portal.FileChooser=kde"}
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
        hyprsaver # Shader-based layer-shell screensaver
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
  // {
    options = lib.recursiveUpdate enabledModule.options {
      myModules.hyprland.portable = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = ''
          Avoid host-specific monitor policy and nonessential session daemons
          while retaining the normal Hyprland package, config, and utilities.
        '';
      };
    };
  }
