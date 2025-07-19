{ lib, pkgs, config, inputs, forEachUser, ... }:
{
  imports = [
    ../configuration.nix
  ];
  services.xserver.enable = true;
  environment.systemPackages = with pkgs; [
    sublime
    vlc
  ];
  myModules.desktop.enable = false;
  myModules.plasma.enable = false;
  imalison.nixOverlay.enable = false;
  myModules.wsl.enable = true;

  networking.hostName = "ryzen-shine-wsl";

  wsl.defaultUser = "imalison";
  system.stateVersion = "22.05";

  home-manager.users = forEachUser {
    home.stateVersion = "22.05";
  };
}
