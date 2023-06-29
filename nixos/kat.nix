{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    bitwarden
    obsidian
  ];
}