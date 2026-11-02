{
  config,
  keys,
  lib,
  pkgs,
  ...
}: let
  groups = import ../groups.nix pkgs;
  cfg = config.myUsers.kat;
in {
  imports = [./git-sync.nix];

  options.myUsers.kat = (import ../lib.nix lib).mkUser "kat" [
    {options.apps.enable = lib.mkEnableOption "Kat's desktop applications";}
  ];

  config = lib.mkMerge [
    (lib.mkIf cfg.enable {
      users.users.kat =
        groups.userDefaults
        // {
          extraGroups = groups.guestGroupsWithWheel;
          name = "kat";
          openssh.authorizedKeys.keys = keys.userKeys.kat;
        };
    })
    (lib.mkIf cfg.apps.enable {
      environment.systemPackages = with pkgs; [
        obsidian
        bitwarden-desktop
        obsidian
      ];

      environment.extraInit = ''
        export PAGER=cat
      '';
    })
  ];
}
