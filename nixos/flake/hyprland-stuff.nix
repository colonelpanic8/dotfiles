{
  pkgs,
  inputs,
  lib ? pkgs.lib,
  hyprlandConfigDir ? ../../dotfiles/config/hypr,
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
  hyprlandConfigSyntax = import ../checks/hyprland-config-syntax {
    inherit pkgs hyprlandConfigDir;
  };
  hyprlandUtilityPackages = with pkgs; [
    hyprpaper
    neowall
    hypridle
    hyprlock
    hyprcursor
    wl-clipboard
    wtype
    cliphist
    grim
    slurp
    swappy
    nwg-displays
    mpvpaper
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
    hyprlandGccIceOverlay
    hyprlandPackage
    hyprlandPluginPackages
    hyprlandStuff
    hyprlandUtilityPackages
    hyprtasking
    hyprwinview
    hyprexpo
    llvmNmCompat
    ;
}
