{
  inputs,
  pkgs,
  lib,
  ...
}: let
  evaDesktop = inputs.eva.packages.${pkgs.stdenv.hostPlatform.system}.eva-desktop;
in {
  config = lib.mkIf (pkgs.stdenv.hostPlatform.system == "x86_64-linux") {
    environment.systemPackages = [evaDesktop];

    home-manager.sharedModules = [
      {
        systemd.user.services.eva = {
          Unit = {
            Description = "EVA desktop assistant";
            After = ["graphical-session.target"];
            PartOf = ["graphical-session.target"];
          };
          Service = {
            ExecStart = "${lib.getExe evaDesktop} tray";
            Restart = "on-abnormal";
            RestartSec = 2;
          };
          Install.WantedBy = ["graphical-session.target"];
        };
      }
    ];
  };
}
