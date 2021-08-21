{
  imports = [
    ../internet-computer.nix
    ../raspberry-pi.nix
    ../base.nix
  ];

  networking.hostName = "air-gapped-pi";
  hardware.video.hidpi.enable = true;

  system.stateVersion = "21.05";
}
