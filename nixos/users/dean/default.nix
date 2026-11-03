{
  config,
  keys,
  lib,
  pkgs,
  ...
}: let
  groups = import ../groups.nix pkgs;
in {
  options.myUsers.dean = (import ../../../nix-shared/users/lib.nix lib).mkUser "dean" [];

  config = lib.mkIf config.myUsers.dean.enable {
    users.users.dean =
      groups.userDefaults
      // {
        extraGroups = groups.guestGroupsWithWheel;
        name = "dean";
        openssh.authorizedKeys.keys = keys.userKeys.dean;
      };
  };
}
