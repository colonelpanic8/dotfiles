{config, ...}: {
  home-manager.users.imalison.xdg.configFile."ccusage-fleet/config.json".text = import ../nix-shared/ccusage-fleet-config.nix {
    localHost = config.networking.hostName;
  };
}
