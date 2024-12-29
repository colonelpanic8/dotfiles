{ config, lib, forEachUser, ... }:
{
  imports = [
    ./android.nix
    ./base.nix
    ./ben.nix
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
    ./k3s.nix
    ./kat.nix
    ./keybase.nix
    ./kubelet.nix
    ./laptop.nix
    ./nix.nix
    # ./nixified.ai.nix
    ./nvidia.nix
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
    myModules.base.enable = true;
    myModules.desktop.enable = true;
    myModules.plasma.enable = true;
    myModules.gnome.enable = false;
    myModules.xmonad.enable = true;
    myModules.extra.enable = true;
    myModules.electron.enable = true;
    myModules.code.enable = true;
    myModules.games.enable = true;
    myModules.syncthing.enable = true;
    myModules.fonts.enable = true;
    myModules.nixified-ai.enable = false;
  };
}
