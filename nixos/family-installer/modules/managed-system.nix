{
  config,
  lib,
  ...
}: let
  cfg = config.familyInstaller;
  updateFlake = "${cfg.updates.source}#${config.networking.hostName}";
in {
  options.familyInstaller.updates = {
    enable = lib.mkEnableOption "updates after this machine is registered in the family release";
    source = lib.mkOption {
      type = lib.types.str;
      default = "git+https://github.com/colonelpanic8/dotfiles?dir=nixos/family-installer&ref=family-stable";
      description = "Published installer repository and tested release ref.";
    };
  };

  config = {
    system.autoUpgrade = {
      enable = cfg.updates.enable;
      flake = updateFlake;
      flags = lib.mkForce ["--refresh" "--flake" (lib.escapeShellArg updateFlake)];
      dates = "daily";
      randomizedDelaySec = "45m";
      allowReboot = false;
      upgrade = false;
    };
    boot.loader.systemd-boot.configurationLimit = lib.mkDefault 10;
    boot.loader.grub.configurationLimit = lib.mkDefault 10;
  };
}
