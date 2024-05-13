{ lib, pkgs, config, inputs, forEachUser, ... }:
{
  imports = [
    ../configuration.nix
  ];
  services.xserver.enable = true;
  environment.systemPackages = with pkgs; [sublime];
  modules.desktop.enable = false;
  modules.plasma.enable = false;
  imalison.nixOverlay.enable = false;
  modules.wsl.enable = true;

  networking.hostName = "dean-zephyrus";

  wsl.defaultUser = "dean";
  system.stateVersion = "22.05";

  home-manager.users = forEachUser {
    home.stateVersion = "22.05";
  };
}
