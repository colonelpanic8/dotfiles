{ pkgs, ... }:
{
  imports = [
    ./arm-incompatible.nix
    ./base.nix
    ./code.nix
    ./desktop.nix
    ./internet-computer.nix
    ./syncthing.nix
    ./xmonad.nix
  ];
}
