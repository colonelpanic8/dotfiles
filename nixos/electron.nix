{ pkgs, config, makeEnable, forEachUser, ... }:
makeEnable config "myModules.electron" false {
  environment.systemPackages = with pkgs; [
    element-desktop
    # bitwarden
    discord
    # etcher
    # keybase-gui
    zoom-us
  ];
  home-manager.users = forEachUser (if pkgs.stdenv.hostPlatform.system == "x86_64-linux" then {
    # systemd.user.services.bitwarden = {
    #   Unit = {
    #     Description = "Bitwarden";
    #     After = [ "graphical-session-pre.target" "tray.target" ];
    #     PartOf = [ "graphical-session.target" ];
    #   };

    #   Install = { WantedBy = [ "graphical-session.target" ]; };

    #   Service = {
    #     ExecStart = "${pkgs.bitwarden}/bin/bitwarden";
    #     Restart = "always";
    #     RestartSec = 3;
    #   };
    # };
  } else {});
}
