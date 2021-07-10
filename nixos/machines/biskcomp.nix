{
  imports = [
    ../raspberry-pi.nix
  ];

  networking.hostName = "biskcomp";
  hardware.video.hidpi.enable = true;
}
