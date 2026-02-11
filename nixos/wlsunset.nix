{ config, pkgs, lib, makeEnable, ... }:
makeEnable config "myModules.wlsunset" true {
  home-manager.sharedModules = [
    {
      services.wlsunset = {
        enable = true;
        latitude = 37.7;
        longitude = -122.4;
        temperature = {
          day = 6500;
          night = 4000;
        };
        systemdTarget = "hyprland-session.target";
      };
    }
  ];
}
