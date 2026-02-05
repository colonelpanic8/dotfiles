{ config, lib, makeEnable, inputs, ... }:
makeEnable config "myModules.quickshell" false {
  home-manager.sharedModules = [
    inputs.caelestia-shell.homeManagerModules.default
    ({ lib, nixos, ... }:
      let
        hyprlandEnabled = lib.attrByPath ["myModules" "hyprland" "enable"] false nixos.config;
      in
      {
        programs.caelestia = {
          enable = true;
          cli.enable = true;
          systemd.target = lib.mkDefault (
            if hyprlandEnabled
            then "wayland-session@Hyprland.target"
            else "graphical-session.target"
          );
        };
      })
  ];
}
