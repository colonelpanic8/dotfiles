{
  config,
  keys,
  lib,
  pkgs,
  ...
}: let
  groups = import ../groups.nix pkgs;
in {
  imports = [
    ./dotfiles.nix
    ./secrets.nix
    ./fleet.nix
    ./git-sync.nix
    ./google-drive.nix
    ./vaultwarden.nix
  ];

  options.myUsers.imalison = (import ../../../nix-shared/users/lib.nix lib).mkUser "imalison" [];

  config = lib.mkIf config.myUsers.imalison.enable {
    users.users.imalison =
      groups.userDefaults
      // {
        extraGroups = groups.extraGroups ++ ["wheel" "dialout"];
        name = "imalison";
        openssh.authorizedKeys.keys = keys.userKeys.imalison;
      };

    security.sudo.extraRules = [
      {
        users = ["imalison"];
        commands = [
          {
            command = "ALL";
            options = ["NOPASSWD" "SETENV"];
          }
        ];
      }
    ];
  };
}
