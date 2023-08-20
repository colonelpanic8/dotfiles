{ pkgs, ... }:
{
  imports = [
    ./android.nix
    ./base.nix
    ./code.nix
    ./desktop.nix
    ./extra.nix
    ./games.nix
    ./git-sync.nix
    ./internet-computer.nix
    ./keybase.nix
    ./nixified.ai.nix
    ./syncthing.nix
    ./xmonad.nix
  ];
}
