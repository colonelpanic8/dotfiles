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

    assertions =
      [
        {
          assertion = config.myUsers.${config.myModules.primaryUser}.has.dotfiles or false;
          message = "myModules.primaryUser (${config.myModules.primaryUser}) must be a myUsers account at level dotfiles or full.";
        }
      ]
      ++ lib.mapAttrsToList (name: user: {
        assertion = user.has.dotfiles || !(config.home-manager.users ? ${name});
        message = "home-manager.users.${name} is defined, but myUsers.${name} is disabled or below the dotfiles level. Gate that module on config.myUsers.${name}.has.dotfiles.";
      })
      config.myUsers;

    security.sudo.wheelNeedsPassword = true;

    home-manager.users = lib.mapAttrs (_: _: {}) (lib.filterAttrs (_: user: user.has.dotfiles) config.myUsers);

    nix.sshServe = {
      enable = true;
      keys = keys.allKeys;
    };
  };
}
