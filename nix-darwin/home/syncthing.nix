{
  config,
  lib,
  pkgs,
  ...
}: let
  shared = import ../../nix-shared/syncthing.nix;
  inherit (shared) devices allDevices;
in {
  home.activation.ensureSyncthingDirectories = lib.hm.dag.entryAfter ["writeBoundary"] ''
    /bin/mkdir -p "$HOME/Library/Logs/Syncthing" "$HOME/sync" "$HOME/railbird"
  '';

  services.syncthing = {
    enable = true;
    package = pkgs.syncthing;
    settings = {
      inherit devices;
      folders = {
        sync = {
          path = "~/sync";
          devices = allDevices;
          ignorePerms = true;
        };
        railbird = {
          path = "~/railbird";
          devices = allDevices;
          ignorePerms = true;
        };
      };
      options = {
        relaysEnabled = true;
        localAnnounceEnabled = true;
      };
    };
  };
}
