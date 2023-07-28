{ lib, pkgs, config, inputs, ... }:
{
  imports = [
    ../wsl.nix
    ../kat.nix
  ];

  networking.hostName = "jay-lenovo-wsl";

  wsl.defaultUser = "kat";
  system.stateVersion = "22.05";
}
