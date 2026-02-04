{ lib, pkgs, config, inputs, ... }:
{
  imports = [
    ../configuration.nix
  ];

  imalison.nixOverlay.enable = false;
  myModules.wsl.enable = true;

  networking.hostName = "jay-lenovo-wsl";

  wsl.defaultUser = "kat";
  system.stateVersion = "22.05";

  home-manager.sharedModules = [
    {
      home.stateVersion = "22.05";
    }
  ];
}
