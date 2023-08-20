{ lib, pkgs, config, inputs, ... }:
{
  imports = [
    ../configuration.nix
  ];

  modules.wls.enable = true;tworking.hostName = "jay-lenovo-wsl";

  wsl.defaultUser = "kat";
  system.stateVersion = "22.05";
}
