{
  config,
  lib,
  ...
}: let
  cfg = config.myModules.bootloaders.grubWindows;
  systemdBootCfg = config.myModules.bootloaders.systemdBoot;
in {
  options.myModules.bootloaders.grubWindows = {
    enable = lib.mkEnableOption "GRUB with Windows chainloading support";

    configurationLimit = lib.mkOption {
      default = 5;
      type = lib.types.int;
      description = "Maximum number of NixOS configurations shown in GRUB.";
    };

    windowsEfiUuid = lib.mkOption {
      default = null;
      type = lib.types.nullOr lib.types.str;
      description = ''
        Filesystem UUID of the Windows EFI system partition. When set, GRUB
        gets an explicit Windows Boot Manager chainload entry in addition to
        entries discovered by os-prober.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = !systemdBootCfg.enable;
        message = ''
          myModules.bootloaders.grubWindows.enable conflicts with
          myModules.bootloaders.systemdBoot.enable. Disable systemdBoot before
          enabling the GRUB Windows boot strategy.
        '';
      }
      {
        assertion = builtins.hasAttr "/boot" config.fileSystems;
        message = "The GRUB Windows boot strategy expects an EFI filesystem mounted at /boot.";
      }
    ];

    boot.loader = {
      efi = {
        canTouchEfiVariables = lib.mkDefault true;
        efiSysMountPoint = lib.mkDefault "/boot";
      };

      grub = {
        enable = true;
        efiSupport = true;
        device = "nodev";
        useOSProber = true;
        configurationLimit = cfg.configurationLimit;
        timeoutStyle = "menu";
        extraEntries = lib.optionalString (cfg.windowsEfiUuid != null) ''
          menuentry "Windows Boot Manager" {
            insmod part_gpt
            insmod fat
            search --no-floppy --fs-uuid --set=windows_esp ${cfg.windowsEfiUuid}
            chainloader ($windows_esp)/EFI/Microsoft/Boot/bootmgfw.efi
          }
        '';
      };
    };
  };
}
