{ pkgs, ... }:
{
  imports = [
    ./android.nix
    ./arm-incompatible.nix
    ./games.nix
    ./desktop.nix
    ./xmonad.nix
    ./code.nix
    ./extra.nix
    ./internet-computer.nix
  ];
}
