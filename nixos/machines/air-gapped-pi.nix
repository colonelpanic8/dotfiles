{
  imports = [
    ../internet-computer.nix
    ../raspberry-pi.nix
    ../base.nix
  ];

  networking.hostName = "air-gapped-pi";
  hardware.video.hidpi.enable = true;
  networking.enable = false;

  system.stateVersion = "21.05";
}
