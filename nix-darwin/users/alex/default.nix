{
  config,
  lib,
  ...
}: let
  cfg = config.myUsers.alex;
in {
  options.myUsers.alex = (import ../../../nix-shared/users/lib.nix lib).mkUser "alex" [];

  config = lib.mkMerge [
    (lib.mkIf cfg.enable {
      users.users.alex = {
        name = "alex";
        home = "/Users/alex";
        openssh.authorizedKeys.keys = (import ../../../nix-shared/keys.nix).userKeys.alex;
      };
    })
    (lib.mkIf cfg.has.dotfiles {
      home-manager.users.alex.dotfiles.gitIdentity = {
        name = "Alexander Malison";
        email = "alexmalison@gmail.com";
      };
    })
  ];
}
