{
  config,
  keys,
  lib,
  pkgs,
  ...
}: let
  groups = import ../groups.nix pkgs;
in {
  options.myUsers.loewy = (import ../../../nix-shared/users/lib.nix lib).mkUser "loewy" [];

  config = lib.mkIf config.myUsers.loewy.enable {
    users.users.loewy =
      groups.userDefaults
      // {
        extraGroups = groups.guestGroups;
        name = "loewy";
        openssh.authorizedKeys.keys = keys.userKeys.loewy;
      };
  };
}
