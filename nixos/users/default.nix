{
  config,
  keys,
  lib,
  ...
}: {
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

    assertions = [
      {
        assertion = config.myUsers.${config.myModules.primaryUser}.has.dotfiles or false;
        message = "myModules.primaryUser (${config.myModules.primaryUser}) must be a myUsers account at level dotfiles or full.";
      }
    ];

    security.sudo.wheelNeedsPassword = true;

    home-manager.users = lib.mapAttrs (_: _: {}) (lib.filterAttrs (_: user: user.has.dotfiles) config.myUsers);

    nix.sshServe = {
      enable = true;
      keys = keys.allKeys;
    };
  };
}
