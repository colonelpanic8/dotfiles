{
  lib,
  pkgs,
  config,
  inputs,
  ...
}: {
  imports = [
    ../configuration.nix
  ];
  services.xserver.enable = true;
  environment.systemPackages = with pkgs; [sublime];
  myModules.desktop.enable = false;
  myModules.plasma.enable = false;
  imalison.nixOverlay.enable = false;
  myModules.wsl.enable = true;

  networking.hostName = "dean-zephyrus";
  myModules.hostIdentity = {
    emoticon = "🌬️";
    tmux.background = "#0284c7";
  };

  wsl.defaultUser = "dean";
  system.stateVersion = "22.05";

  home-manager.sharedModules = [
    {
      home.stateVersion = "22.05";
    }
  ];
}
