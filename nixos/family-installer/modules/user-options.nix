{lib, ...}: {
  options = {
    myModules.primaryUser = lib.mkOption {
      type = lib.types.str;
      default = "imalison";
      description = ''
        Login name of the account this machine primarily belongs to. Modules
        that configure a single human user's services should read this instead
        of naming an account directly.
      '';
    };
  };
}
