{ lib, ... }: {
  options = {
    myModules.xmonad.picom.vSync.enable = lib.mkOption {
      default = true;
      type = lib.types.bool;
    };
  };
}
