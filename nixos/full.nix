{ pkgs, ... }:
{
  imports = [
    ./android.nix
    ./arm-incompatible.nix
    ./games.nix
    ./desktop.nix
    ./code.nix
    ./extra.nix
    ./internet-computer.nix
  ];
}
