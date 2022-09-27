{ pkgs, ... }:
{
  imports = [
    ./all.nix
    ./android.nix
    ./base.nix
    # ./cachix.nix
    ./code.nix
    ./essential-no-arm.nix
    ./extra.nix
    ./internet-computer.nix
    ./dfinity.nix
  ];
}
