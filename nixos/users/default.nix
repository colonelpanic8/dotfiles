{
  config,
  keys,
  lib,
  ...
}: let
  usersLib = import ../../nix-shared/users/lib.nix lib;
in {
  imports = [
    ./imalison
    ./kat
    ./dean
    ./alex
    ./loewy
    ./ben
  ];

  config = {
    myUsers.${config.myModules.primaryUser}.enable = lib.mkDefault true;

    assertions = usersLib.assertions {
      inherit config;
      primaryUser = config.myModules.primaryUser;
      primaryUserOption = "myModules.primaryUser";
    };

    security.sudo.wheelNeedsPassword = true;

    home-manager.users = lib.genAttrs (usersLib.homeUsers config) (_: {});

    nix.sshServe = {
      enable = true;
      keys = keys.allKeys;
    };
  };
}
