{ config, lib, forEachUser, ... }:
{
  imports = [
    ./android.nix
    ./base.nix
    ./cache-server.nix
    ./cache.nix
    ./code.nix
    ./desktop.nix
    ./electron.nix
    ./environment.nix
    ./essential.nix
    ./extra.nix
    ./games.nix
    ./git-sync.nix
    ./gitea-runner.nix
    ./gitea.nix
    ./gnome.nix
    ./imalison.nix
    ./internet-computer.nix
    ./kat.nix
    ./keybase.nix
    ./nix.nix
    ./nixified.ai.nix
    ./options.nix
    ./plasma.nix
    ./postgres.nix
    ./rabbitmq.nix
    ./secrets.nix
    ./ssh.nix
    ./syncthing.nix
    ./user-specific.nix
    ./users.nix
    ./vscode.nix
    ./wsl.nix
    ./xmonad.nix
  ];

  options = {
    features.full.enable = lib.mkEnableOption "Do everything";
  };

  config = lib.mkIf config.features.full.enable {
    modules.base.enable = true;
    modules.desktop.enable = true;
    modules.plasma.enable = true;
    modules.gnome.enable = false;
    modules.xmonad.enable = true;
    modules.extra.enable = true;
    modules.electron.enable = true;
    modules.code.enable = true;
    modules.games.enable = true;
    modules.syncthing.enable = true;
    modules.fonts.enable = true;
    modules.nixified-ai.enable = false;
  };
}
