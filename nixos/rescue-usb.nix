{
  lib,
  pkgs,
  ...
}: let
  rescueDisk = "/dev/disk/by-id/usb-USB_SanDisk_3.2Gen1_0401aacece19e891d0b7a8d566c93ac11ad16e1492dcf0212e85f3013fa2e2ad81d400000000000000000000e8960d0f00861b18a9558107ce2d0e9d-0:0";
in {
  boot.initrd.availableKernelModules = [
    "ehci_pci"
    "sd_mod"
    "uas"
    "usb_storage"
    "xhci_pci"
  ];
  boot.loader = {
    efi = {
      canTouchEfiVariables = false;
      efiSysMountPoint = "/boot";
    };
    grub = {
      enable = true;
      device = rescueDisk;
      efiSupport = true;
      efiInstallAsRemovable = true;
      configurationLimit = 5;

      # memtest86+ 8.00. It reports failing physical addresses but cannot map
      # them to a DIMM slot, so MemTest86 below is the one to reach for first.
      memtest86.enable = true;

      extraFiles."memtest86.efi" = "${pkgs.memtest86-efi}/BOOTX64.efi";

      extraEntries = ''
        menuentry "MemTest86 (PassMark, free)" {
          chainloader /memtest86.efi
        }
        menuentry "Memtest86+ (nosmp, unattended)" {
          linux @bootRoot@/memtest.bin nosmp nobench nopause
        }
      '';
    };
  };

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/d59ef500-d65c-4dbe-9f4a-dbd54c1885b1";
    fsType = "ext4";
    options = ["noatime"];
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-label/NIXRESCUE";
    fsType = "vfat";
    options = [
      "fmask=0022"
      "dmask=0022"
    ];
  };

  swapDevices = [];
  services.fstrim.enable = true;
  zramSwap.enable = true;

  # The writable USB is intentionally portable rather than tied to one host.
  networking.usePredictableInterfaceNames = lib.mkDefault true;
}
