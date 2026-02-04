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
        # Hyper = Ctrl+Alt+Meta+Shift while held
        "hyper:C-A-M-S" = { };
      };
    };

    # TODO: Add per-keyboard overrides here once device IDs are known.
    # Example:
    # keyboards.externalKinesis = {
    #   ids = [ "1ea7:0907" ];
    #   settings = {
    #     main = {
    #       leftalt = "leftmeta";
    #       leftmeta = "leftalt";
    #       rightalt = "layer(hyper)";
    #     };
    #     "hyper:C-A-M-S" = { };
    #   };
    # };
  };
}
