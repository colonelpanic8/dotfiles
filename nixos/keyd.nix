{ config, lib, makeEnable, ... }:
makeEnable config "myModules.keyd" true {
  services.keyd = {
    enable = true;

    # Base remap applied to all keyboards.
    keyboards.default = {
      # Exclude the Glove80 (MoErgo) by vendor:product ID.
      ids = [ "*" "-16c0:27db" ];
      settings = {
        main = {
          # Caps Lock -> Control
          capslock = "leftcontrol";
          # Swap Left Alt and Left Super
          leftalt = "leftmeta";
          leftmeta = "leftalt";
          # Right Alt -> Hyper chord
          rightalt = "layer(hyper)";
        };
        # Hyper = Ctrl+Alt+Meta while held (matches $hyper = SUPER CTRL ALT)
        "hyper:C-A-M" = { };
      };
    };

    # MoErgo Glove80: only map Right Meta/Super to Hyper.
    keyboards.moErgo = {
      ids = [ "16c0:27db" ];
      settings = {
        main = {
          rightmeta = "layer(hyper)";
        };
        # Hyper = Ctrl+Alt+Meta while held (matches $hyper = SUPER CTRL ALT)
        "hyper:C-A-M" = { };
      };
    };
  };
}
