{ pkgs, ... }:
{
  imports = [
    ./android.nix
    ./arm-incompatible.nix
    ./base.nix
    ./code.nix
    ./desktop.nix
    ./extra.nix
    ./games.nix
    ./internet-computer.nix
    ./syncthing.nix
    ./xmonad.nix
  ];
}
