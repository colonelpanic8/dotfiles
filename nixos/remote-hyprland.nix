{
  config,
  lib,
  pkgs,
  makeEnable,
  ...
}: let
  cfg = config.myModules.remote-hyprland;
  hyprlandPackage = config.programs.hyprland.package;
  hyprlandUnwrapped = hyprlandPackage.passthru.unwrapped or hyprlandPackage;
  geometry = "${toString cfg.width}x${toString cfg.height}@${toString cfg.refreshRate}";
  monitorRule = "${cfg.output},${geometry},0x0,${toString cfg.scale}";
  # A Lua config without a monitor rule gets Hyprland's automatic scale, which
  # picks 2 for the remote output. Load it, then pin the configured geometry.
  luaConfigWithMonitor = pkgs.writeText "remote-hyprland.lua" ''
    dofile(${builtins.toJSON (toString cfg.configFile)})
    hl.monitor({ output = "", mode = "${geometry}", position = "0x0", scale = ${toString cfg.scale} })
  '';
  hyprlandConfigFile =
    if cfg.configFile == null
    then remoteHyprlandConfig
    else if lib.hasSuffix ".lua" (toString cfg.configFile)
    then luaConfigWithMonitor
    else cfg.configFile;
  # wayvnc 0.10 waits for an output power "on" event before capturing, which
  # neither Hyprland nor headless sway ever sends, so no frames are delivered.
  wayvncPackage = pkgs.wayvnc.overrideAttrs (old: {
    postPatch =
      (old.postPatch or "")
      + ''
        substituteInPlace src/main.c --replace-fail \
          "int rc = image_source_acquire_power_on(self->image_source);" \
          "int rc = -1;"
      '';
  });
  # Hyprland's nested Wayland backend binds xdg_wm_base at version 6, which
  # wlroots compositors (version 5) reject. Bind whatever the host offers.
  aquamarineForNesting = (lib.findFirst (p: (p.pname or "") == "aquamarine")
    (throw "remote-hyprland: aquamarine not found in Hyprland's buildInputs")
    hyprlandUnwrapped.buildInputs).overrideAttrs (old: {
    postPatch =
      (old.postPatch or "")
      + ''
        substituteInPlace src/backend/Wayland.cpp --replace-fail \
          "&xdg_wm_base_interface, 6)" \
          "&xdg_wm_base_interface, std::min<uint32_t>(version, 6))"
      '';
  });
  nestedHyprlandBin = pkgs.writeShellScript "remote-hyprland-nested-bin" ''
    echo $$ > "$XDG_RUNTIME_DIR/remote-hyprland.pid"
    export LD_LIBRARY_PATH=${aquamarineForNesting.out}/lib
    exec ${hyprlandUnwrapped}/bin/Hyprland "$@"
  '';
  nestedHyprland = pkgs.writeShellScript "remote-hyprland-nested" ''
    # sway sets these to its own name for its children.
    export XDG_CURRENT_DESKTOP=Hyprland XDG_SESSION_DESKTOP=Hyprland
    # Only the host may open libinput. With a usable seat, Hyprland would read
    # every device itself, physical ones included, alongside the host's copy.
    unset LIBSEAT_BACKEND
    exec ${hyprlandUnwrapped}/bin/start-hyprland --path ${nestedHyprlandBin} -- --config ${hyprlandConfigFile}
  '';
  # An invisible headless wlroots host lets Hyprland run without DRM master,
  # alongside the display manager and any physical session on the same GPU.
  nestedHostConfig = pkgs.writeText "remote-hyprland-host.conf" ''
    output HEADLESS-1 resolution ${toString cfg.width}x${toString cfg.height} bg #000000 solid_color
    default_border none
    default_floating_border none
    for_window [app_id=".*"] fullscreen enable
    # Nested Hyprland only sees input forwarded by this host. Take the uinput
    # devices that ydotool and computer-use-linux synthesize, but never the
    # physical keyboard and mouse, which belong to the local session.
    input * events disabled
    input "9011:26214:ydotoold_virtual_device" events enabled
    input "4660:22136:computer-use-linux_absolute_pointer" events enabled
    exec ${nestedHyprland}
  '';
  # Runs a command against this service's Hyprland instance.
  remoteHyprlandWithInstance = pkgs.writeShellScript "remote-hyprland-with-instance" ''
        set -euo pipefail

        export XDG_CURRENT_DESKTOP=Hyprland
        export XDG_SESSION_DESKTOP=Hyprland
        export XDG_SESSION_TYPE=wayland
        export LIBSEAT_BACKEND=seatd

        wanted_pid=""
        for _ in $(${pkgs.coreutils}/bin/seq 1 50); do
          ${lib.optionalString cfg.nested ''
      # Pick this service's nested instance, not a physical session's.
      wanted_pid="$(${pkgs.coreutils}/bin/cat "$XDG_RUNTIME_DIR/remote-hyprland.pid" 2>/dev/null || echo none)"
    ''}
          instance="$(
            ${hyprlandPackage}/bin/hyprctl instances \
              | ${pkgs.gawk}/bin/awk -v wanted_pid="$wanted_pid" '
                  /^instance / {
                    sig = $2
                    sub(/:$/, "", sig)
                  }
                  /^[[:space:]]*time:/ { time = $2 }
                  /^[[:space:]]*pid:/ { pid = $2 }
                  /^[[:space:]]*wl socket:/ {
                    if (sig != "" && time != "" && (wanted_pid == "" || pid == wanted_pid)) {
                      print time " " sig " " $3
                    }
                  }
                ' \
              | ${pkgs.coreutils}/bin/sort -n \
              | ${pkgs.coreutils}/bin/tail -n 1
          )"

          if [ -n "$instance" ]; then
            read -r _ HYPRLAND_INSTANCE_SIGNATURE WAYLAND_DISPLAY <<EOF
    $instance
    EOF
            export HYPRLAND_INSTANCE_SIGNATURE WAYLAND_DISPLAY
            break
          fi
          ${pkgs.coreutils}/bin/sleep 0.1
        done

        if [ -z "''${HYPRLAND_INSTANCE_SIGNATURE:-}" ] || [ -z "''${WAYLAND_DISPLAY:-}" ]; then
          echo "Timed out waiting for a Hyprland instance" >&2
          exit 1
        fi

        for _ in $(${pkgs.coreutils}/bin/seq 1 50); do
          if ${hyprlandPackage}/bin/hyprctl -i "$HYPRLAND_INSTANCE_SIGNATURE" -j monitors >/dev/null 2>&1; then
            break
          fi
          ${pkgs.coreutils}/bin/sleep 0.1
        done

        exec "$@"
  '';
  remoteHyprlandStartVnc = pkgs.writeShellScript "remote-hyprland-start-vnc" ''
    ${
      if cfg.nested
      then ''
        # The nested instance has a single output sized by the host.
        exec ${wayvncPackage}/bin/wayvnc \
          --log-level=info \
          --socket="$XDG_RUNTIME_DIR/wayvncctl-remote-hyprland" \
          ${cfg.bindAddress} ${toString cfg.port}
      ''
      else ''
        # Give wayvnc a stable output name instead of relying on Hyprland's
        # fallback HEADLESS-* naming.
        ${hyprlandPackage}/bin/hyprctl -i "$HYPRLAND_INSTANCE_SIGNATURE" output create headless ${cfg.output} >/dev/null 2>&1 || true
        ${hyprlandPackage}/bin/hyprctl -i "$HYPRLAND_INSTANCE_SIGNATURE" keyword monitor '${monitorRule}' >/dev/null 2>&1 || true

        exec ${wayvncPackage}/bin/wayvnc \
          --log-level=info \
          --output ${cfg.output} \
          ${cfg.bindAddress} ${toString cfg.port}
      ''
    }
  '';
  remoteHyprlandConfig = pkgs.writeText "remote-hyprland.conf" ''
    monitor=${monitorRule}
    monitor=,${geometry},0x0,${toString cfg.scale}

    env = XDG_CURRENT_DESKTOP,Hyprland
    env = XDG_SESSION_DESKTOP,Hyprland
    env = XDG_SESSION_TYPE,wayland

    input {
      kb_layout = us
      follow_mouse = 1
    }

    general {
      gaps_in = 4
      gaps_out = 8
      border_size = 2
      layout = dwindle
    }

    decoration {
      rounding = 4
    }

    dwindle {
      pseudotile = true
      preserve_split = true
    }

    misc {
      disable_hyprland_logo = true
      disable_splash_rendering = true
      force_default_wallpaper = 0
    }

    $mainMod = SUPER
    bind = $mainMod, Return, exec, ${cfg.terminalCommand}
    bind = $mainMod, D, exec, ${pkgs.rofi}/bin/rofi -show drun
    bind = $mainMod, Q, killactive
    bind = $mainMod SHIFT, M, exit

    bind = $mainMod, H, movefocus, l
    bind = $mainMod, J, movefocus, d
    bind = $mainMod, K, movefocus, u
    bind = $mainMod, L, movefocus, r

    bind = $mainMod SHIFT, H, movewindow, l
    bind = $mainMod SHIFT, J, movewindow, d
    bind = $mainMod SHIFT, K, movewindow, u
    bind = $mainMod SHIFT, L, movewindow, r

    bind = $mainMod, 1, workspace, 1
    bind = $mainMod, 2, workspace, 2
    bind = $mainMod, 3, workspace, 3
    bind = $mainMod, 4, workspace, 4
    bind = $mainMod, 5, workspace, 5
    bind = $mainMod, 6, workspace, 6
    bind = $mainMod, 7, workspace, 7
    bind = $mainMod, 8, workspace, 8
    bind = $mainMod, 9, workspace, 9

    bind = $mainMod SHIFT, 1, movetoworkspace, 1
    bind = $mainMod SHIFT, 2, movetoworkspace, 2
    bind = $mainMod SHIFT, 3, movetoworkspace, 3
    bind = $mainMod SHIFT, 4, movetoworkspace, 4
    bind = $mainMod SHIFT, 5, movetoworkspace, 5
    bind = $mainMod SHIFT, 6, movetoworkspace, 6
    bind = $mainMod SHIFT, 7, movetoworkspace, 7
    bind = $mainMod SHIFT, 8, movetoworkspace, 8
    bind = $mainMod SHIFT, 9, movetoworkspace, 9

    exec-once = ${cfg.terminalCommand}
  '';
  servicePath = lib.makeBinPath [
    pkgs.coreutils
    pkgs.gnugrep
    pkgs.gnused
    pkgs.systemd
  ];
  autostartInstall = lib.optionalAttrs cfg.autoStart {
    Install = {
      WantedBy = ["default.target"];
    };
  };
  enabledModule = makeEnable config "myModules.remote-hyprland" false {
    myModules.hyprland.enable = true;

    services.seatd = lib.mkIf (!cfg.nested) {
      enable = true;
      group = "video";
    };

    # seatd's upstream unit is Type=notify wrapped in sdnotify-wrapper, but the
    # readiness notification never reaches systemd on this host, so it hits the
    # 90s start timeout and restart-loops forever (counter climbed past 50),
    # tearing down /run/seatd.sock on every cycle and breaking any libseat
    # client (e.g. a headless Hyprland). seatd creates its socket immediately on
    # startup, so Type=simple is sufficient and stays active.
    systemd.services.seatd = lib.mkIf (!cfg.nested) {
      serviceConfig.Type = lib.mkForce "simple";
    };

    users.manageLingering = true;
    users.users.${cfg.user} = {
      linger = true;
      # The nested host opens the synthetic uinput devices itself.
      extraGroups = lib.mkIf cfg.nested ["input"];
    };

    environment.systemPackages = [wayvncPackage];

    home-manager.users.${cfg.user} = {
      systemd.user.services = {
        remote-hyprland =
          {
            Unit = {
              Description = "Headless Hyprland session for remote VNC access";
              After = ["default.target"];
              # Bring VNC and the shell back whenever the session (re)starts.
              Wants = ["remote-hyprland-wayvnc.service"] ++ lib.optional (cfg.shellCommand != null) "remote-hyprland-shell.service";
            };
            Service =
              if cfg.nested
              then {
                ExecStart = "${pkgs.sway}/bin/sway --config ${nestedHostConfig}";
                Restart = "on-failure";
                RestartSec = 5;
                # start-hyprland aborts in its SIGTERM handler and leaves the
                # nested Hyprland running without a host.
                TimeoutStopSec = 10;
                UnsetEnvironment = ["WAYLAND_DISPLAY" "DISPLAY" "HYPRLAND_INSTANCE_SIGNATURE"];
                Environment = [
                  "WLR_BACKENDS=headless,libinput"
                  "WLR_LIBINPUT_NO_DEVICES=1"
                  # libinput needs a session; noop opens /dev/input directly
                  # (the user is in the input group) without a logind seat.
                  "LIBSEAT_BACKEND=noop"
                  "WLR_RENDERER=gles2"
                  "WLR_RENDER_DRM_DEVICE=${cfg.renderNode}"
                  "XDG_CURRENT_DESKTOP=Hyprland"
                  "XDG_SESSION_DESKTOP=Hyprland"
                  "XDG_SESSION_TYPE=wayland"
                  # Keep this instance out of the user manager's session environment.
                  "HYPRLAND_NO_SD_NOTIFY=1"
                  "HYPRLAND_NO_SD_VARS=1"
                  "HYPRLAND_NO_SD_TARGET=1"
                  "IMALISON_REMOTE_HYPRLAND=1"
                  "PATH=/run/wrappers/bin:/etc/profiles/per-user/${cfg.user}/bin:/run/current-system/sw/bin:${servicePath}"
                ];
              }
              else {
                ExecStart = "${hyprlandPackage}/bin/start-hyprland --path ${hyprlandPackage}/bin/Hyprland -- --config ${hyprlandConfigFile}";
                Restart = "on-failure";
                RestartSec = 5;
                Environment = [
                  "XDG_CURRENT_DESKTOP=Hyprland"
                  "XDG_SESSION_DESKTOP=Hyprland"
                  "XDG_SESSION_TYPE=wayland"
                  "LIBSEAT_BACKEND=seatd"
                  "PATH=${servicePath}"
                ];
              };
          }
          // autostartInstall;

        remote-hyprland-wayvnc = {
          Unit = {
            Description = "VNC server for the headless Hyprland session";
            After = ["remote-hyprland.service"];
            BindsTo = ["remote-hyprland.service"];
          };
          Service = {
            ExecStart = "${remoteHyprlandWithInstance} ${remoteHyprlandStartVnc}";
            Restart = "always";
            RestartSec = 5;
            Environment = [
              "XDG_CURRENT_DESKTOP=Hyprland"
              "XDG_SESSION_DESKTOP=Hyprland"
              "XDG_SESSION_TYPE=wayland"
              "LIBSEAT_BACKEND=seatd"
              "PATH=${servicePath}"
            ];
          };
        };

        remote-hyprland-shell = lib.mkIf (cfg.shellCommand != null) {
          Unit = {
            Description = "Desktop shell for the headless Hyprland session";
            After = ["remote-hyprland.service"];
            BindsTo = ["remote-hyprland.service"];
          };
          Service = {
            ExecStart = "${remoteHyprlandWithInstance} ${cfg.shellCommand}";
            Restart = "always";
            RestartSec = 3;
            Environment =
              [
                "XDG_CURRENT_DESKTOP=Hyprland"
                "XDG_SESSION_DESKTOP=Hyprland"
                "XDG_SESSION_TYPE=wayland"
              ]
              ++ cfg.shellEnvironment;
          };
        };
      };
    };
  };
in
  enabledModule
  // {
    options = lib.recursiveUpdate enabledModule.options {
      myModules.remote-hyprland = {
        user = lib.mkOption {
          type = lib.types.str;
          default = "imalison";
          description = "User account that owns the remote Hyprland session.";
        };

        bindAddress = lib.mkOption {
          type = lib.types.str;
          default = "127.0.0.1";
          description = "Address for wayvnc to bind. Keep localhost when using SSH or Tailscale forwarding.";
        };

        port = lib.mkOption {
          type = lib.types.port;
          default = 5900;
          description = "TCP port for wayvnc.";
        };

        output = lib.mkOption {
          type = lib.types.str;
          default = "remote";
          description = "Stable Hyprland headless output name captured by wayvnc.";
        };

        width = lib.mkOption {
          type = lib.types.ints.positive;
          default = 1920;
          description = "Remote output width.";
        };

        height = lib.mkOption {
          type = lib.types.ints.positive;
          default = 1080;
          description = "Remote output height.";
        };

        refreshRate = lib.mkOption {
          type = lib.types.ints.positive;
          default = 60;
          description = "Remote output refresh rate.";
        };

        scale = lib.mkOption {
          type = lib.types.number;
          default = 1;
          description = "Remote output scale.";
        };

        terminalCommand = lib.mkOption {
          type = lib.types.str;
          default = "${pkgs.ghostty}/bin/ghostty --gtk-single-instance=false";
          description = "Command launched for the default terminal binding and initial window.";
        };

        autoStart = lib.mkOption {
          type = lib.types.bool;
          default = false;
          description = ''
            Whether to start the remote Hyprland session automatically with the
            user's systemd manager. Unless `nested` is set, keep this disabled on
            single-GPU hosts with an active display manager, because Hyprland
            needs DRM master.
          '';
        };

        nested = lib.mkOption {
          type = lib.types.bool;
          default = false;
          description = ''
            Run Hyprland as the fullscreen client of an invisible headless sway
            that renders on `renderNode`. This needs neither seatd nor DRM
            master, so it coexists with a display manager and a physical
            session on the same GPU.
          '';
        };

        renderNode = lib.mkOption {
          type = lib.types.str;
          default = "/dev/dri/renderD128";
          description = "DRM render node used by the nested session's host compositor.";
        };

        shellCommand = lib.mkOption {
          type = lib.types.nullOr lib.types.str;
          default = null;
          description = "Desktop shell to run inside the remote session, bound to its lifetime.";
        };

        shellEnvironment = lib.mkOption {
          type = lib.types.listOf lib.types.str;
          default = [];
          description = "Extra `Environment=` entries for the remote session's shell.";
        };

        configFile = lib.mkOption {
          type = lib.types.nullOr (lib.types.either lib.types.path lib.types.str);
          default = null;
          example = "/home/alice/.config/hypr/hyprland.lua";
          description = "Hyprland config to load instead of the built-in minimal one.";
        };
      };
    };
  }
