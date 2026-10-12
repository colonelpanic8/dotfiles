{
  config,
  pkgs,
  makeEnable,
  realUsers,
  ...
}:
makeEnable config "myModules.displayManager" false {
  services.accounts-daemon.enable = true;

  # The greeter's PipeWire grabs ALSA devices at boot, so a lingering user's
  # WirePlumber probes them while busy and drops the analog outputs.
  systemd.user = let
    notGreeter.unitConfig.ConditionUser = "!sddm";
  in {
    services = {
      pipewire = notGreeter;
      pipewire-pulse = notGreeter;
      wireplumber = notGreeter;
    };
    sockets = {
      pipewire = notGreeter;
      pipewire-pulse = notGreeter;
    };
  };
  services.displayManager.sddm = {
    enable = true;
    settings = {
      Users = {
        # Limit login candidates to regular interactive users.
        MaximumUid = 60000;
        MinimumUid = 1000;
        RememberLastUser = true;
      };
      Theme = {
        # Breeze hides the user chooser when users exceed this threshold.
        # Keep it aligned with declared normal users so the list stays visible.
        DisableAvatarsThreshold = (builtins.length realUsers) + 10;
      };
    };
    extraPackages = with pkgs; [
      # sddm-astronaut
    ];
  };
}
