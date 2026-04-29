{
  config,
  inputs,
  lib,
  makeEnable,
  pkgs,
  ...
}:
let
  system = pkgs.stdenv.hostPlatform.system;
  noctaliaPackage = inputs.noctalia.packages.${system}.default;
  waitForWayland = pkgs.writeShellScript "noctalia-wait-for-wayland" ''
    runtime_dir="''${XDG_RUNTIME_DIR:-/run/user/$(${pkgs.coreutils}/bin/id -u)}"

    for _ in $(${pkgs.coreutils}/bin/seq 1 50); do
      if [ -n "''${WAYLAND_DISPLAY:-}" ] && [ -S "$runtime_dir/$WAYLAND_DISPLAY" ]; then
        exit 0
      fi
      ${pkgs.coreutils}/bin/sleep 0.1
    done

    echo "Wayland socket not ready: WAYLAND_DISPLAY=''${WAYLAND_DISPLAY:-<unset>} XDG_RUNTIME_DIR=$runtime_dir" >&2
    exit 1
  '';
in
makeEnable config "myModules.noctalia" false {
  environment.systemPackages = [
    noctaliaPackage
  ];

  # Noctalia's battery widget talks to UPower. Hosts that deliberately do not
  # have batteries can still override this back to false.
  services.upower.enable = lib.mkDefault true;

  home-manager.sharedModules = [
    inputs.noctalia.homeModules.default
    ({ lib, ... }: {
      programs.noctalia-shell = {
        enable = true;
        # This module provides the Hyprland-scoped service below.
        systemd.enable = false;
      };

      home.activation.noctaliaLauncherOverviewLayer =
        lib.hm.dag.entryAfter [ "writeBoundary" ] ''
          settings_file="$HOME/.config/noctalia/settings.json"
          settings_tmp="$(${pkgs.coreutils}/bin/mktemp)"

          ${pkgs.coreutils}/bin/mkdir -p "$(${pkgs.coreutils}/bin/dirname "$settings_file")"

          if [ -e "$settings_file" ] && ${lib.getExe pkgs.jq} -e . "$settings_file" >/dev/null 2>&1; then
            ${lib.getExe pkgs.jq} \
              '.appLauncher = (.appLauncher // {}) | .appLauncher.overviewLayer = true' \
              "$settings_file" > "$settings_tmp"
            ${pkgs.coreutils}/bin/mv "$settings_tmp" "$settings_file"
          else
            ${pkgs.coreutils}/bin/printf '%s\n' \
              '{' \
              '  "appLauncher": {' \
              '    "overviewLayer": true' \
              '  }' \
              '}' > "$settings_file"
            ${pkgs.coreutils}/bin/rm -f "$settings_tmp"
          fi
        '';

      systemd.user.services.noctalia-shell = {
        Unit = {
          Description = "Noctalia Shell";
          Documentation = "https://docs.noctalia.dev";
          PartOf = [ "hyprland-session.target" ];
          After = [ "hyprland-session.target" ];
        };

        Service = {
          ExecCondition = "/run/current-system/sw/bin/desktop_shell_ui exec-condition noctalia";
          ExecStartPre = "${waitForWayland}";
          ExecStart = "${lib.getExe noctaliaPackage} --no-duplicate";
          Restart = "on-failure";
          RestartSec = 1;
        };

        Install.WantedBy = [ "hyprland-session.target" ];
      };
    })
  ];
}
