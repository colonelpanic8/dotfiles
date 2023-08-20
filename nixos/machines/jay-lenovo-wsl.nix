{ lib, pkgs, config, inputs, forEachUser, ... }:
{
  imports = [
    ../configuration.nix
  ];

  modules.wls.enable = true;tworking.hostName = "jay-lenovo-wsl";

  wsl.defaultUser = "kat";
  system.stateVersion = "22.05";

  home-manager.users = forEachUser {
    home.stateVersion = "22.05";
  };
}
