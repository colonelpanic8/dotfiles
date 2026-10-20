{
  config,
  lib,
  pkgs,
  ...
}: let
  primaryUser = config.myModules.primaryUser;
  owner = config.myModules.userProfile.owner;
in {
  imports = [./user-options.nix];

  options.myModules.userProfile.owner = lib.mkOption {
    type = lib.types.enum ["ivan" "other"];
    default = "other";
    description = "Whose personal defaults to use, independently of the login name.";
  };

  config = {
    myModules.primaryUser = lib.mkDefault (
      if owner == "ivan"
      then "imalison"
      else "user"
    );

    nix.settings.experimental-features = ["nix-command" "flakes"];
    networking.firewall.enable = lib.mkDefault true;
    programs.zsh.enable = true;
    users.users.${primaryUser} = {
      isNormalUser = true;
      shell = pkgs.zsh;
      extraGroups = ["wheel" "networkmanager"];
    };

    home-manager.useGlobalPkgs = true;
    home-manager.useUserPackages = true;
    home-manager.users.${primaryUser} = {
      home.stateVersion = lib.mkDefault config.system.stateVersion;
      programs.zsh = {
        enable = true;
        shellAliases = import ./multiplexer-aliases.nix;
      };
      programs.tmux.enable = true;
      programs.git = {
        enable = true;
        settings.user = lib.mkIf (owner == "ivan") {
          name = "Ivan Malison";
          email = "IvanMalison@gmail.com";
        };
      };
      home.packages = [pkgs.zellij];
    };
  };
}
