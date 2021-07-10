{
  imports = [
    ../raspberry-pi.nix
    ../plex.nix
  ];

  networking.hostName = "biskcomp";
  hardware.video.hidpi.enable = true;
}
