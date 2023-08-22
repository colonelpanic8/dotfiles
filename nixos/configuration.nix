{ config, lib, ... }:
{
  imports = [
    ./android.nix
    ./base.nix
    ./cache.nix
    ./code.nix
    ./desktop.nix
    ./environment.nix
    ./essential.nix
    ./extra.nix
    ./games.nix
    ./git-sync.nix
    ./internet-computer.nix
    ./keybase.nix
    ./nix.nix
    ./nixified.ai.nix
    ./options.nix
    ./secrets.nix
    ./ssh.nix
    ./syncthing.nix
    ./users.nix
    ./wsl.nix
    ./xmonad.nix
  ];

  options = {
    features.full.enable = lib.mkEnableOption "Do everything";
  };

  config = lib.mkIf config.features.full.enable {
    modules.base.enable = true;
    modules.desktop.enable = true;
    modules.xmonad.enable = true;
    modules.extra.enable = true;
    modules.code.enable = true;
    modules.games.enable = true;
    modules.syncthing.enable = true;
    modules.fonts.enable = true;
    modules.nixified-ai.enable = true;
  };
}
