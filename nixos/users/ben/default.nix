{
  config,
  keys,
  lib,
  pkgs,
  ...
}: let
  groups = import ../groups.nix pkgs;
  cfg = config.myUsers.ben;
in {
  options.myUsers.ben = (import ../../../nix-shared/users/lib.nix lib).mkUser "ben" [];

  config = lib.mkMerge [
    (lib.mkIf cfg.enable {
      users.users.ben =
        groups.userDefaults
        // {
          extraGroups = groups.guestGroups;
          name = "ben";
          openssh.authorizedKeys.keys = keys.userKeys.ben;
        };
    })
    (lib.mkIf cfg.has.dotfiles {
      home-manager.users.ben = {
        programs.zsh = {
          enable = true;
          shellAliases = {
            l = "ls -CF";
            la = "ls -A";
            ll = "ls -lh";
            lla = "ls -alh";
            ls = "ls --color=auto";
            gts = "git status";
            gtl = "git log";
          };
        };
      };
    })
  ];
}
