{
  config,
  lib,
  ...
}: let
  cfg = config.myModules.bootloaders.grub;
  systemdBootCfg = config.myModules.bootloaders.systemdBoot;
in {
  options.myModules.bootloaders.grub = {
    enable = lib.mkEnableOption "GRUB bootloader support";

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
        gets an explicit Windows Boot Manager chainload entry and skips
        os-prober autodetection.
      '';
    };

    theme = lib.mkOption {
      default = null;
      type = lib.types.nullOr lib.types.path;
      description = "GRUB theme directory.";
    };

    gfxmode = lib.mkOption {
      default = "auto";
      type = lib.types.str;
      description = "GRUB graphical mode used for EFI and BIOS.";
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = !systemdBootCfg.enable;
        message = ''
          myModules.bootloaders.grub.enable conflicts with
          myModules.bootloaders.systemdBoot.enable. Disable systemdBoot before
          enabling the GRUB boot strategy.
        '';
      }
      {
        assertion = builtins.hasAttr "/boot" config.fileSystems;
        message = "The GRUB boot strategy expects an EFI filesystem mounted at /boot.";
      }
    ];

    boot.loader = {
      efi = {
        canTouchEfiVariables = lib.mkDefault true;
        efiSysMountPoint = lib.mkDefault "/boot";
      };

      grub = {
        enable = true;
        memtest86.enable = true;
        efiSupport = true;
        device = "nodev";
        useOSProber = cfg.windowsEfiUuid == null;
        configurationLimit = cfg.configurationLimit;
        timeoutStyle = "menu";
        theme = lib.mkIf (cfg.theme != null) (lib.mkDefault cfg.theme);
        gfxmodeEfi = lib.mkDefault cfg.gfxmode;
        gfxmodeBios = lib.mkDefault cfg.gfxmode;
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
