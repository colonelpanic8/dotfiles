{
  makeEnable,
  config,
  ...
}: let
  shared = import ../nix-shared/syncthing.nix;
  inherit (shared) devices allDevices;
in
  makeEnable config "myModules.syncthing" true {
    system.activationScripts.syncthingPermissions = {
      text = ''
        chown -R syncthing:syncthing /var/lib/syncthing
        chmod -R 2770 /var/lib/syncthing
        mkdir -p /var/lib/syncthing/sync
        mkdir -p /var/lib/syncthing/railbird
      '';
    };
    systemd.services.syncthing = {
      serviceConfig = {
        AmbientCapabilities = "CAP_CHOWN";
        CapabilityBoundingSet = "CAP_CHOWN";
      };
    };
    services.syncthing = {
      enable = true;
      settings = {
        inherit devices;
        folders = {
          sync = {
            path = "~/sync";
            devices = allDevices;
            ignorePerms = true;
            copyOwnershipFromParent = true;
          };
          railbird = {
            path = "~/railbird";
            devices = allDevices;
            ignorePerms = true;
            copyOwnershipFromParent = true;
          };
        };
        options = {
          relaysEnabled = true;
          localAnnounceEnabled = true;
        };
      };
    };
  }
