{ lib, pkgs, config, inputs, ... }:
{
  imports = [
    ../wsl.nix
    ../kat.nix
  ];

  wsl.defaultUser = "kat";
  system.stateVersion = "22.05";
}
