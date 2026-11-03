{
  config,
  lib,
  ...
}: let
  cfg = config.myUsers.imalison;
in {
  options.myUsers.imalison = (import ../../../nix-shared/users/lib.nix lib).mkUser "imalison" [];

  config = lib.mkMerge [
    (lib.mkIf cfg.enable {
      users.users.imalison = {
        name = "imalison";
        home = "/Users/imalison";
        description = "Ivan Malison";
        openssh.authorizedKeys.keys = (import ../../../nix-shared/keys.nix).userKeys.imalison;
      };
    })
    (lib.mkIf cfg.has.full {
      home-manager.users.imalison.imports = [./personal.nix];
    })
  ];
}
