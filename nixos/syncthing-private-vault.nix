{
  config,
  inputs,
  lib,
  ...
}: {
  imports = [inputs.gocryptfs-tray.nixosModules.default];

  config = lib.mkIf config.myModules.syncthing.enable {
    services.gocryptfs-tray.vaults.private = {
      user = "imalison";
      cipherUser = "syncthing";
      cipherDir = "/var/lib/syncthing/sync/Private.encrypted";
      link = "/home/imalison/Private";
      secretCommand = ["pass" "show" "syncthing/private-vault"];
      gocryptfsArgs = ["-sharedstorage"];
      syncthingIgnore.folder = "/var/lib/syncthing/sync";
      # syncthingPermissions chmods all of /var/lib/syncthing to 2770.
      activationDeps = ["syncthingPermissions"];
    };
  };
}
