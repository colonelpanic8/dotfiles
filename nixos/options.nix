{ lib, ... }: {
  options = {
    modules.xmonad.picom.vSync.enable = lib.makeOption {
      default = true;
      type = lib.types.bool;
    };
  };
}
