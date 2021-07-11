{ pkgs, ... }:
{
  imports = [
    ./all.nix
    ./android.nix
    ./base.nix
    ./code.nix
    ./essential-no-arm.nix
    ./internet-computer.nix
  ];
}
