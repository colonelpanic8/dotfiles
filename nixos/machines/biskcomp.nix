{
  imports = [
    ../raspberry-pi.nix
    ../jellyfin.nix
    # ../internet-computer.nix
  ];

  networking.hostName = "biskcomp";
  hardware.video.hidpi.enable = true;

  system.stateVersion = "21.05";
}
