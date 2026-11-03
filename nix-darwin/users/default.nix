{
  config,
  lib,
  ...
}: let
  usersLib = import ../../nix-shared/users/lib.nix lib;
  homeUsers = usersLib.homeUsers config;
in {
  imports = [
    ./imalison
    ./kat
    ./alex
  ];

  config = {
    myUsers.${config.system.primaryUser}.enable = lib.mkDefault true;

    assertions = usersLib.assertions {
      inherit config;
      primaryUser = config.system.primaryUser;
      primaryUserOption = "system.primaryUser";
    };

    home-manager.users = lib.genAttrs homeUsers (_: {
      imports = [
        ../home/agents.nix
        ../home/desktop.nix
      ];
    });

    # Home Manager refuses to activate until the user's Nix profile
    # directory exists, which a fresh account does not have yet.
    system.activationScripts.preActivation.text =
      lib.concatMapStrings (user: ''
        /usr/bin/sudo -u ${user} /bin/mkdir -p ${config.users.users.${user}.home}/.local/state/nix/profiles
      '')
      homeUsers;
  };
}
