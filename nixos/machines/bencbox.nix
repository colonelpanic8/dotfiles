{ lib, pkgs, config, inputs, ... }:
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

  networking.hostName = "bencbox";

  wsl.defaultUser = "ben";
  system.stateVersion = "22.05";

  home-manager.sharedModules = [
    {
      home.stateVersion = "22.05";
    }
  ];

  users.users.ben = {
    extraGroups = [
      "audio"
      "adbusers"
      "disk"
      "docker"
      "networkmanager"
      "openrazer"
      "plugdev"
      "syncthing"
      "systemd-journal"
      "video"
    ] ++ ["wheel"];
  };
}
