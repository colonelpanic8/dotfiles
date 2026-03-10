{ config, lib, makeEnable, inputs, ... }:
makeEnable config "myModules.quickshell" false {
  home-manager.sharedModules = [
    inputs.caelestia-shell.homeManagerModules.default
    ({ lib, nixos, pkgs, ... }:
      let
        hyprlandEnabled = lib.attrByPath ["myModules" "hyprland" "enable"] false nixos.config;
        system = pkgs.stdenv.hostPlatform.system;
        caelestiaPackage = inputs.caelestia-shell.packages.${system}.with-cli.override {
          # Caelestia pins app2unit 1.0.3, which no longer builds on this nixpkgs snapshot.
          app2unit = pkgs.app2unit;
        };
      in
      {
        programs.caelestia = {
          enable = true;
          package = caelestiaPackage;
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
