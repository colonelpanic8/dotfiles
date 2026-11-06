{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [
    ./agent-thread-backup.nix
    ./agent-workspace.nix
    ./android.nix
    ./base.nix
    ./bootloaders/grub.nix
    ./bootloaders/systemd-boot.nix
    ./cache-server.nix
    ./cache.nix
    ./cargo-sweep.nix
    ./chrome-favicon-dbus.nix
    ./nix-gcroot-sweep.nix
    ./ccusage-fleet.nix
    ./claude-mcp.nix
    ./claude-remote-control.nix
    ./code.nix
    ./computer-use-linux.nix
    ./cua.nix
    ./desktop.nix
    ./dux-calendar.nix
    ./dux-shell.nix
    ./electron.nix
    ./environment.nix
    ../nix-shared/system/essential.nix
    ./extra.nix
    ./games.nix
    ./scrobble-scrubber.nix
    ./gitea-runner.nix
    ./gitea.nix
    ./google-calendar.nix
    ./google-messages.nix
    ./gnome.nix
    ./home-assistant.nix
    ./hermes-agent.nix
    ./input-resilience.nix
    ./internet-computer.nix
    ./k3s.nix
    ./keepbook-sync.nix
    ./keybase.nix
    ./kubelet.nix
    ./laptop.nix
    ./nix.nix
    ./notifications-tray-icon.nix
    ./ntfy.nix
    ./nvidia.nix
    ./options.nix
    ./paseo.nix
    ./plasma.nix
    ./postgres.nix
    ./primary-user-home.nix
    ./rabbitmq.nix
    ./remote-hyprland.nix
    ./river-xmonad.nix
    ./ssh.nix
    ./sni.nix
    ./syncthing.nix
    ./syncthing-private-vault.nix
    ./taffybar.nix
    ./t3code-server.nix
    ./tailscale.nix
    ./kokoro.nix
    ./users
    ./vscode.nix
    ./voxtype.nix
    ./waybar.nix
    ./wsl.nix
    ./wyoming.nix
    ./xmonad.nix
    ./org-agenda-api-host.nix
  ];

  options = {
    features.full.enable = lib.mkEnableOption "Do everything";
  };

  config = lib.mkMerge [
    {
      environment.systemPackages = with pkgs; [
        roborock-control
        menu-roborock
      ];
      system.autoUpgrade.flake = "github:colonelpanic8/dotfiles?dir=nixos#${config.networking.hostName}";
      myUsers =
        {imalison.enable = lib.mkDefault true;}
        // lib.genAttrs ["kat" "dean" "alex" "loewy" "ben"] (_: {
          enable = lib.mkDefault true;
          level = lib.mkDefault "full";
        });
    }
    (lib.mkIf config.features.full.enable {
      myModules.base.enable = true;
      myModules.desktop.enable = true;
      myModules.plasma.enable = true;
      myModules.gnome.enable = false;
      myModules.xmonad.enable = true;
      myModules.extra.enable = true;
      myModules.electron.enable = true;
      myModules.code.enable = true;
      myModules.games.enable = false;
      myModules."keepbook-sync".enable = config.myUsers.imalison.has.full;
      myModules.syncthing.enable = true;
      myModules.fonts.enable = true;
    })
  ];
}
