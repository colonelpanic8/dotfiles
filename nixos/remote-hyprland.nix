{
  config,
  lib,
  pkgs,
  makeEnable,
  ...
}: let
  cfg = config.myModules.remote-hyprland;
  hyprlandPackage = config.programs.hyprland.package;
  geometry = "${toString cfg.width}x${toString cfg.height}@${toString cfg.refreshRate}";
  monitorRule = "${cfg.output},${geometry},0x0,${toString cfg.scale}";
  remoteHyprlandStartVnc = pkgs.writeShellScript "remote-hyprland-start-vnc" ''
        set -euo pipefail

        export XDG_CURRENT_DESKTOP=Hyprland
        export XDG_SESSION_DESKTOP=Hyprland
        export XDG_SESSION_TYPE=wayland
        export LIBSEAT_BACKEND=seatd

        for _ in $(${pkgs.coreutils}/bin/seq 1 50); do
          instance="$(
            ${hyprlandPackage}/bin/hyprctl instances \
              | ${pkgs.gawk}/bin/awk '
                  /^instance / {
                    sig = $2
                    sub(/:$/, "", sig)
                  }
                  /^[[:space:]]*time:/ { time = $2 }
                  /^[[:space:]]*wl socket:/ {
                    if (sig != "" && time != "") {
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

        # Give wayvnc a stable output name instead of relying on Hyprland's
        # fallback HEADLESS-* naming.
        ${hyprlandPackage}/bin/hyprctl -i "$HYPRLAND_INSTANCE_SIGNATURE" output create headless ${cfg.output} >/dev/null 2>&1 || true
        ${hyprlandPackage}/bin/hyprctl -i "$HYPRLAND_INSTANCE_SIGNATURE" keyword monitor '${monitorRule}' >/dev/null 2>&1 || true

        exec ${pkgs.wayvnc}/bin/wayvnc \
          --log-level=info \
          --output ${cfg.output} \
          ${cfg.bindAddress} ${toString cfg.port}
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

    services.seatd = {
      enable = true;
      group = "video";
    };

    users.manageLingering = true;
    users.users.${cfg.user}.linger = true;

    environment.systemPackages = [pkgs.wayvnc];

    home-manager.users.${cfg.user} = {
      systemd.user.services = {
        remote-hyprland =
          {
            Unit = {
              Description = "Headless Hyprland session for remote VNC access";
              After = ["default.target"];
            };
            Service = {
              ExecStart = "${hyprlandPackage}/bin/start-hyprland --path ${hyprlandPackage}/bin/Hyprland -- --config ${remoteHyprlandConfig}";
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

        remote-hyprland-wayvnc =
          {
            Unit = {
              Description = "VNC server for the headless Hyprland session";
              After = ["remote-hyprland.service"];
              Requires = ["remote-hyprland.service"];
              PartOf = ["remote-hyprland.service"];
            };
            Service = {
              ExecStart = "${remoteHyprlandStartVnc}";
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
            user's systemd manager. Keep this disabled on single-GPU hosts with
            an active display manager, because Hyprland needs DRM master.
          '';
        };
      };
    };
  }
