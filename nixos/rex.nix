{
  config,
  inputs,
  lib,
  ...
}: let
  cfg = config.myModules.rex;
in {
  options.myModules.rex = {
    enable = lib.mkEnableOption "personal Rex servers";

    users = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [config.myModules.primaryUser];
      defaultText = lib.literalExpression "[ config.myModules.primaryUser ]";
      description = "Existing users to run persistent personal Rex servers for.";
    };
  };

  config = lib.mkIf cfg.enable {
    assertions =
      map (user: {
        assertion = config.users.users.${user}.isNormalUser or false;
        message = "myModules.rex.users: ${user} must be an existing normal user.";
      })
      cfg.users;

    users.users = lib.genAttrs cfg.users (_: {linger = true;});

    home-manager.users = lib.genAttrs cfg.users (_: {
      imports = [inputs.rex.homeManagerModules.default];
      programs.rex.enable = true;
      services.rex.enable = true;
    });
  };
}
