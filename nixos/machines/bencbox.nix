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
  modules.desktop.enable = false;
  modules.plasma.enable = false;
  imalison.nixOverlay.enable = false;
  modules.wsl.enable = true;

  networking.hostName = "bencbox";

  wsl.defaultUser = "ben";
  system.stateVersion = "22.05";

  home-manager.users = forEachUser {
    home.stateVersion = "22.05";
  };
}
