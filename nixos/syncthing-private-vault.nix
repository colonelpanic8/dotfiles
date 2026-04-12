{
  config,
  lib,
  pkgs,
  ...
}: let
  cipherDir = "/var/lib/syncthing/sync/Private.encrypted";
  mountPoint = "/home/imalison/Private";
in
  lib.mkIf config.myModules.syncthing.enable {
    system.activationScripts.syncthingPrivateVault = {
      text = ''
        install -d -o syncthing -g syncthing -m 2770 ${lib.escapeShellArg cipherDir}
      '';
    };

    home-manager.users.imalison = {lib, ...}: {
      home.packages = [pkgs.gocryptfs];

      home.activation.ensureSyncthingPrivateVaultMountpoint = lib.hm.dag.entryAfter ["writeBoundary"] ''
        mkdir -p ${lib.escapeShellArg mountPoint}
      '';
    };
  }
