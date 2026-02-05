{ config, pkgs, makeEnable, ... }:
makeEnable config "myModules.waybar" false {
  home-manager.sharedModules = [
    {
      programs.waybar.enable = true;

      systemd.user.services.waybar = {
        Unit = {
          Description = "Waybar";
          PartOf = [ "wayland-session@Hyprland.target" "tray.target" ];
          After = [ "wayland-session@Hyprland.target" "tray.target" ];
          Wants = [ "tray.target" ];
        };
        Service = {
          ExecStartPre = "${pkgs.bash}/bin/bash -lc 'uid=$(id -u); runtime_dir=\"$XDG_RUNTIME_DIR\"; if [ -z \"$runtime_dir\" ]; then runtime_dir=\"/run/user/$uid\"; fi; for i in $(seq 1 50); do if [ -n \"$WAYLAND_DISPLAY\" ] && [ -S \"$runtime_dir/$WAYLAND_DISPLAY\" ]; then found=1; break; fi; sleep 0.1; done; if [ -z \"$found\" ]; then exit 1; fi; \"$HOME/.config/waybar/scripts/render-config\"'";
          ExecStart = "${pkgs.waybar}/bin/waybar -c %t/waybar/config.jsonc -s %t/waybar/style.css";
          Restart = "always";
          RestartSec = 1;
        };
        Install = {
          WantedBy = [ "wayland-session@Hyprland.target" ];
        };
      };
    }
  ];
}
