{ config, lib, makeEnable, inputs, ... }:
makeEnable config "myModules.quickshell" false {
  home-manager.sharedModules = [
    inputs.caelestia-shell.homeManagerModules.default
    ({ lib, nixos, pkgs, ... }:
      let
        hyprlandEnabled = lib.attrByPath ["myModules" "hyprland" "enable"] false nixos.config;
        system = pkgs.stdenv.hostPlatform.system;
        quickshellBase = inputs.caelestia-shell.inputs.quickshell.packages.${system}.default.override {
          # GCC 15.2 ICEs on generated NetworkManager DBus sources in
          # quickshell 0.2.1. Quickshell's own dev shell uses clangStdenv.
          stdenv = pkgs.clangStdenv;
          withX11 = false;
          withI3 = false;
        };
        quickshellUnwrapped = quickshellBase.unwrapped.overrideAttrs (old: {
          # Clang then needs PCH disabled because quickshell's PipeWire target
          # adds compile flags that do not match the shared PCH target.
          cmakeFlags = (old.cmakeFlags or []) ++ [ (lib.cmakeBool "NO_PCH" true) ];
        });
        quickshellPackage = quickshellUnwrapped.stdenv.mkDerivation {
          inherit (quickshellUnwrapped) version meta buildInputs;
          pname = "${quickshellUnwrapped.pname}-wrapped";
          nativeBuildInputs = quickshellUnwrapped.nativeBuildInputs ++ [ pkgs.qt6.wrapQtAppsHook ];
          dontUnpack = true;
          dontConfigure = true;
          dontBuild = true;
          installPhase = ''
            mkdir -p $out
            cp -r ${quickshellUnwrapped}/* $out
          '';
          passthru.unwrapped = quickshellUnwrapped;
        };
        caelestiaPackage = inputs.caelestia-shell.packages.${system}.with-cli.override {
          # Caelestia pins app2unit 1.0.3, which no longer builds on this nixpkgs snapshot.
          app2unit = pkgs.app2unit;
          quickshell = quickshellPackage;
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
