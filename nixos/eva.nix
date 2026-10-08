{
  inputs,
  config,
  pkgs,
  lib,
  ...
}: let
  evaDesktop = inputs.eva.packages.${pkgs.stdenv.hostPlatform.system}.eva-desktop;
  evaPackage =
    if config.myModules.hyprland.enable
    then
      pkgs.symlinkJoin {
        name = "${evaDesktop.name}-hyprland";
        paths = [evaDesktop];
        nativeBuildInputs = [pkgs.makeWrapper];
        postBuild = ''
          wrapProgram "$out/bin/eva-desktop" --set _JAVA_AWT_WM_NONREPARENTING 1
        '';
        meta = evaDesktop.meta;
      }
    else evaDesktop;
in {
  config = lib.mkIf (pkgs.stdenv.hostPlatform.system == "x86_64-linux") {
    environment.systemPackages = [evaPackage];

    home-manager.sharedModules = [
      {
        systemd.user.services.eva = {
          Unit = {
            Description = "EVA desktop assistant";
            After = ["graphical-session.target"];
            PartOf = ["graphical-session.target"];
          };
          Service = {
            ExecStart = "${lib.getExe evaPackage} tray";
            Restart = "on-abnormal";
            RestartSec = 2;
          };
          Install.WantedBy = ["graphical-session.target"];
        };
      }
    ];
  };
}
