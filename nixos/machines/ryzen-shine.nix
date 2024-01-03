{ config, lib, pkgs, inputs, forEachUser, ... }:

{
  imports = [
    ../configuration.nix
  ];

  features.full.enable = true;
  # Needed for now because monitors have different refresh rates
  modules.xmonad.picom.vSync.enable = false;
  modules.cache-server = {
    enable = true;
    port = 3090;
  };
  modules.gitea-runner.enable = true;
  modules.postgres.enable = true;

  boot.loader.systemd-boot.configurationLimit = 5;

  networking.hostName = "ryzen-shine";

  environment.systemPackages = with pkgs; [
    linuxPackages_latest.perf
  ];

  boot.initrd.systemd.enable = true;
  boot.plymouth = {
    enable = false;
  };

  services.autorandr = {
    enable = true;
    profiles = {
      "1896Office" = {
        fingerprint = {
          DP-0 = "00ffffffffffff0010ace4a153364b300c200104b55123783bfce1b04d3bb8250e505421080001010101010101010101010101010101e77c70a0d0a029503020150829623100001a000000ff0023473749594d78677741413052000000fd0001afffff63010a202020202020000000fc0044656c6c20415733343233445702d8020320f12309070183010000654b04000101e305c000e2006ae6060501634b004ed470a0d0a046503020e50c29623100001a00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000009970127903000f000aa4140e0e07012045000002010d310cdb4dbd13b85b620e01455403013c520101046f0d9f002f801f009f0554004c000400663801046f0d9f002f801f009f0566005e0004008b8101046f0d9f002f801f009f057e007600040000000000000000000000000000000000000000000000000000000000005f90";
          HDMI-0 = "00ffffffffffff0010ac67d0534d39312d1a0103803c2278ee4455a9554d9d260f5054a54b00b300d100714fa9408180778001010101565e00a0a0a029503020350055502100001a000000ff00483759434336423831394d530a000000fc0044454c4c205532373135480a20000000fd0038561e711e000a202020202020019e020322f14f1005040302071601141f1213202122230907078301000065030c001000023a801871382d40582c250055502100001e011d8018711c1620582c250055502100009e011d007251d01e206e28550055502100001e8c0ad08a20e02d10103e9600555021000018483f00ca808030401a50130055502100001e00000094";
        };
        config = {
          DP-0 = {
            enable = true;
            mode = "3440x1440";
            rate = "143.97";
            position = "0x1440";
            primary = true;
          };
          HDMI-0 = {
            enable = true;
            rate = "59.95";
            mode = "2560x1440";
            position = "440x0";
          };
        };
      };
    };
  };

  hardware.enableRedistributableFirmware = true;

  networking.interfaces.enp5s0.useDHCP = true;
  networking.interfaces.wlp4s0.useDHCP = true;

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.initrd.availableKernelModules = [ "nvme" "xhci_pci" "ahci" "usb_storage" "usbhid" "sd_mod" ];
  boot.initrd.luks.devices."cryptroot".device = "/dev/nvme0n1p5";
  boot.initrd.kernelModules = [ "dm-snapshot" ];

  # install nvidia drivers in addition to intel one
  hardware.opengl.extraPackages = [ pkgs.linuxPackages.nvidia_x11.out ];
  hardware.opengl.extraPackages32 = [ pkgs.linuxPackages.nvidia_x11.lib32 ];
  services.xserver = {
    videoDrivers = [ "nvidia" ];
  };

  hardware.nvidia.modesetting.enable = true;

  hardware.opengl.driSupport32Bit = true;

  boot.kernelModules = [ "kvm-amd" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/356173ab-d076-43e0-aeb6-6a6829c4402b";
    fsType = "ext4";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/B270-C7E6";
    fsType = "vfat";
  };

  fileSystems."/shared" = {
    device = "/dev/disk/by-uuid/D4009CE8009CD33A";
    fsType = "ntfs";
    options = [ "nofail" ];
  };

  swapDevices =[
    { device = "/dev/disk/by-uuid/f719b44e-295a-4909-9a60-84f87acb7f77"; }
  ];

  # nix.settings.maxJobs = lib.mkDefault 16;
  # High-DPI console
  console.font = lib.mkDefault "${pkgs.terminus_font}/share/consolefonts/ter-u28n.psf.gz";

  # services.xrdp.enable = true;
  # services.xrdp.defaultWindowManager = "startplasma-x11";
  # services.xrdp.openFirewall = true;

  system.stateVersion = "20.03";
  home-manager.users = forEachUser {
    home.stateVersion = "21.05";
  };

  users.extraUsers.dean.home = "/shared/dean";
}
