{
  config,
  keys,
  lib,
  pkgs,
  ...
}: let
  groups = import ../groups.nix pkgs;
in {
  options.myUsers.alex = (import ../lib.nix lib).mkUser "alex" [];

  config = lib.mkIf config.myUsers.alex.enable {
    users.users.alex =
      groups.userDefaults
      // {
        extraGroups = groups.guestGroupsWithWheel;
        name = "alex";
        openssh.authorizedKeys.keys = keys.userKeys.alex;
      };
  };
}
