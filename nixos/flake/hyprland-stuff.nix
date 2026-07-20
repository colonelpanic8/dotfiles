{
  pkgs,
  inputs,
  lib ? pkgs.lib,
  hyprlandConfigDir ? ../../dotfiles/config/hypr,
}: let
  system = pkgs.stdenv.hostPlatform.system;
  hyprlandInput = inputs.hyprland;
  # GCC 15 ICEs while compiling Hyprland's ConfigManager. GCC 16 builds
  # the unmodified source, avoiding the old Clang-only source compatibility patch.
  baseHyprlandPackage = hyprlandInput.packages.${system}.hyprland.override {
    stdenv = pkgs.gcc16Stdenv;
  };
  hyprlockPackage = inputs.hyprlock.packages.${system}.hyprlock;
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
  enableHyprglass = false;
  hyprglass = pkgs.callPackage ../packages/hyprglass {
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
      exec python3 ${../../dotfiles/lib/bin/hypr_rofi_window} "$@"
    '';
  };
  hyprShellUi = pkgs.writeShellApplication {
    name = "hypr_shell_ui";
    runtimeInputs = [
      pkgs.rofi
      hyprRofiWindow
    ];
    text = ''
      exec ${../../dotfiles/lib/bin/hypr_shell_ui} "$@"
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
      exec ${../../dotfiles/lib/bin/hypr_rofi_layout} "$@"
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
      exec python3 ${../../dotfiles/lib/bin/hypr_rofi_action} "$@"
    '';
  };
  hyprsaver = pkgs.callPackage ../packages/hyprsaver {
    src = inputs.hyprsaver;
  };
  hyprlandConfigSyntax = import ../checks/hyprland-config-syntax {
    inherit pkgs hyprlandConfigDir;
  };
  hyprlandUtilityPackages = with pkgs; [
    hyprpaper
    neowall
    hypridle
    hyprlockPackage
    hyprcursor
    wl-clipboard
    wtype
    cliphist
    grim
    slurp
    swappy
    nwg-displays
    hyprsaver
    ddcutil
    jq
    lua5_4
  ];
  hyprlandStuffPaths =
    hyprlandUtilityPackages
    ++ [
      hyprlandPackage
      llvmNmCompat
      hyprRofiLayout
      hyprRofiAction
      hyprRofiWindow
      hyprShellUi
    ]
    ++ hyprlandPluginPackages;
  hyprlandStuffPathStrings = map builtins.toString (hyprlandStuffPaths ++ [hyprlandConfigSyntax]);
  hyprlandStuff = pkgs.symlinkJoin {
    name = "imalison-hyprland-stuff";
    paths = hyprlandStuffPaths;
    postBuild = ''
      mkdir -p "$out/nix-support"
      printf '%s\n' ${lib.escapeShellArgs hyprlandStuffPathStrings} > "$out/nix-support/hyprland-store-paths"
    '';
    passthru = {
      inherit
        baseHyprlandPackage
        hyprlandPackage
        hyprlandPluginPackages
        hyprlandUtilityPackages
        hyprlandConfigSyntax
        ;
    };
    meta = {
      description = "Ivan Malison's Hyprland package, plugins, scripts, and Lua config syntax check";
      platforms = lib.platforms.linux;
    };
  };
in {
  inherit
    baseHyprlandPackage
    cleanupStaleGraphicalSession
    hyprDynamicCursors
    hyprNStack
    hyprRofiAction
    hyprRofiLayout
    hyprRofiWindow
    hyprShellUi
    hyprWorkspaceHistory
    hyprglass
    hyprlandConfigSyntax
    hyprlandPackage
    hyprlandPluginPackages
    hyprlandStuff
    hyprlandUtilityPackages
    hyprsaver
    hyprtasking
    hyprwinview
    hyprexpo
    llvmNmCompat
    ;
}
