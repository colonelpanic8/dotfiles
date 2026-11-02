{
  config,
  inputs,
  lib,
  pkgs,
  ...
}: let
  cfg = config.myModules.displayManager;
  greeterUser =
    if cfg.greeter == "dms"
    then config.services.greetd.settings.default_session.user
    else "sddm";
in {
  imports = [inputs.dux-greeter.nixosModules.default];

  options.myModules.displayManager = {
    enable = lib.mkEnableOption "the graphical display manager";
    greeter = lib.mkOption {
      type = lib.types.enum ["sddm" "dms"];
      default =
        if config.myModules.hyprland.enable
        then "dms"
        else "sddm";
      defaultText = lib.literalExpression ''if config.myModules.hyprland.enable then "dms" else "sddm"'';
      description = ''
        Login greeter. "dms" runs the Dux Shell greeter under greetd, which
        only launches Wayland sessions.
      '';
    };
  };

  config = lib.mkIf cfg.enable (lib.mkMerge [
    {
      services.accounts-daemon.enable = true;

      # The greeter's PipeWire grabs ALSA devices at boot, so a lingering user's
      # WirePlumber probes them while busy and drops the analog outputs.
      systemd.user = let
        notGreeter.unitConfig.ConditionUser = "!${greeterUser}";
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
    }

    (lib.mkIf (cfg.greeter == "sddm") {
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
            DisableAvatarsThreshold = (builtins.length (builtins.attrNames (lib.filterAttrs (_: user: user.isNormalUser) config.users.users))) + 10;
          };
        };
        extraPackages = with pkgs; [
          # sddm-astronaut
        ];
      };
    })

    (lib.mkIf (cfg.greeter == "dms") {
      assertions = [
        {
          assertion = config.myModules.hyprland.enable;
          message = "myModules.displayManager.greeter = \"dms\" runs the greeter in Hyprland.";
        }
      ];
      programs.dms-greeter = {
        enable = true;
        compositor.name = "hyprland";
        quickshell.package = inputs.quickshell-tray.packages.${pkgs.stdenv.hostPlatform.system}.default;
        configHome = config.users.users.${config.myModules.primaryUser}.home;
      };
    })
  ]);
}
