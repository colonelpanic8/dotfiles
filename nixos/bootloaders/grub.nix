{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.myModules.bootloaders.grub;
  systemdBootCfg = config.myModules.bootloaders.systemdBoot;
  memtest86plusDimmDecode = pkgs.memtest86plus.overrideAttrs (old: {
    version = "9.00-7726853-dimm-decode";
    src = pkgs.fetchFromGitHub {
      owner = "memtest86plus";
      repo = "memtest86plus";
      rev = "7726853667fad43f7ec163a99e41fbb9d90ac092";
      hash = "sha256-dQxiQ6X6hSxVJjDH8FgSyH25lO81mA8b6xnJUk6hV9A=";
    };
    patches = (old.patches or []) ++ [../patches/memtest86plus-amd-zen-dimm-decoding.patch];
  });
in {
  options.myModules.bootloaders.grub = {
    enable = lib.mkEnableOption "GRUB bootloader support";

    configurationLimit = lib.mkOption {
      default = 5;
      type = lib.types.int;
      description = "Maximum number of NixOS configurations shown in GRUB.";
    };

    useOSProber = lib.mkOption {
      default = true;
      type = lib.types.bool;
      description = "Whether to enable os-prober for detecting other bootable operating systems.";
    };

    windowsEfiUuid = lib.mkOption {
      default = null;
      type = lib.types.nullOr lib.types.str;
      description = ''
        Filesystem UUID of the Windows EFI system partition. When set, GRUB
        gets an explicit Windows Boot Manager chainload entry in addition to
        any os-prober autodetection.
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
        useOSProber = cfg.useOSProber;
        configurationLimit = cfg.configurationLimit;
        timeoutStyle = "menu";
        theme = lib.mkIf (cfg.theme != null) (lib.mkDefault cfg.theme);
        gfxmodeEfi = lib.mkDefault cfg.gfxmode;
        gfxmodeBios = lib.mkDefault cfg.gfxmode;
        extraEntries =
          ''
            menuentry "Memtest86+ (AMD Zen DIMM slot decoder)" {
              linux @bootRoot@/memtest-dimm-decode.bin
            }
          ''
          + lib.optionalString (cfg.windowsEfiUuid != null) ''
            menuentry "Windows Boot Manager" {
              insmod part_gpt
              insmod fat
              search --no-floppy --fs-uuid --set=root ${cfg.windowsEfiUuid}
              chainloader /EFI/Microsoft/Boot/bootmgfw.efi
            }
          '';
        extraFiles."memtest-dimm-decode.bin" = memtest86plusDimmDecode.efi;
      };
    };
  };
}
