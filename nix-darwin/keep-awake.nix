{
  config,
  lib,
  ...
}: let
  cfg = config.services.keep-awake;
in {
  options.services.keep-awake = {
    enable = lib.mkEnableOption "a persistent macOS sleep assertion";
    onBattery = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Also prevent idle system sleep while running on battery power.";
    };
  };

  config = lib.mkIf cfg.enable {
    launchd.daemons.keep-awake.serviceConfig = {
      ProgramArguments = [
        "/usr/bin/caffeinate"
        (
          if cfg.onBattery
          then "-is"
          else "-s"
        )
      ];
      RunAtLoad = true;
      KeepAlive = true;
      ProcessType = "Background";
    };
  };
}
