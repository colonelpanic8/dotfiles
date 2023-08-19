{ pkgs, ... }:
{
  imports = [
    ./base.nix
    ./code.nix
    ./desktop.nix
    ./internet-computer.nix
    ./syncthing.nix
    ./xmonad.nix
  ];
}
