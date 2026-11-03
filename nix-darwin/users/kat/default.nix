{
  config,
  lib,
  ...
}: let
  cfg = config.myUsers.kat;
in {
  options.myUsers.kat = (import ../../../nix-shared/users/lib.nix lib).mkUser "kat" [];

  config = lib.mkMerge [
    (lib.mkIf cfg.enable {
      users.users.kat = {
        name = "kat";
        home = "/Users/kat";
        openssh.authorizedKeys.keys = (import ../../../nix-shared/keys.nix).userKeys.kat;
      };
    })
    (lib.mkIf cfg.has.dotfiles {
      home-manager.users.kat.dotfiles.gitIdentity = {
        name = "Kat Huang";
        email = "kkathuang@gmail.com";
      };
    })
  ];
}
