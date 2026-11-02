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
    security.sudo.wheelNeedsPassword = true;

    home-manager.users = lib.mapAttrs (_: _: {}) (lib.filterAttrs (_: user: user.has.dotfiles) config.myUsers);

    nix.sshServe = {
      enable = true;
      keys = keys.allKeys;
    };
  };
}
