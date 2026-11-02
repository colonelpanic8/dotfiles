{
  config,
  lib,
  ...
}: {
  home-manager.users.imalison = lib.mkIf config.myUsers.imalison.has.dotfiles {
    xdg.configFile."ccusage-fleet/config.json".text = import ../nix-shared/ccusage-fleet-config.nix {
      localHost = config.networking.hostName;
    };
  };
}
