{ pkgs, ... }:
{
  imports = [
    ./android.nix
    ./arm-incompatible.nix
    ./base.nix
    ./games.nix
    ./desktop.nix
    ./xmonad.nix
    ./code.nix
    ./extra.nix
    ./internet-computer.nix
  ];
}
