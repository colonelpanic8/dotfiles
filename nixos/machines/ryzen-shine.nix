{
  config,
  inputs,
  lib,
  pkgs,
  ...
}: {
  imports = [
    inputs.grub2-themes.nixosModules.default
    ../configuration.nix
    ../nixified.ai.nix
  ];

  features.full.enable = true;
  myModules.claudeRemoteControl.enable = true;
  myModules.taffybar.aiUsageMode = "both";
  myModules.games.enable = lib.mkForce true;
  myModules.hermesAgent.enable = true;
  myModules.kubelet.enable = false;
  myModules.nvidia.enable = true;
  myModules.paseo.enable = true;
  # Needed for now because monitors have different refresh rates
  myModules.xmonad.picom.vSync.enable = false;
  myModules.cache-server = {
    enable = true;
    port = 3090;
  };
  myModules.gitea-runner.enable = false;
  myModules.postgres.enable = true;
  myModules.voxtype.enable = true;
  myModules.cua = {
    enable = true;
    android = {
      enable = true;
      # Android is QEMU/KVM-only. Keep this manually startable until SVM/AMD-V
      # is exposed to Linux and /dev/kvm exists on this host.
      autoStart = false;
    };
  };
  myModules.railbird-k3s = {
    enable = false;
    serverAddr = "https://jimi-hendnix.local:6443";
    # extraFlags = ["--node-taint preferNoSchedule=true:NoSchedule"];
  };
  services.mullvad-vpn.enable = lib.mkForce false;
  myModules.nixified-ai.enable = true;
  boot.kernelPackages = pkgs.linuxPackages_latest;

  # MemTest86 2026-08-16 (/boot/MemTest86-Report-20260816-215525_934520.html):
  # 193 errors, every one a bit-31 flip, at five addresses inside a 25KB window
  # around 0x2AAC6BDC8 — at JEDEC 2133MT/s, so a defective cell, not an
  # overclock. Reserve 4MB around it so Linux never hands that page out. The
  # backslash keeps GRUB from expanding "$0x..." as a variable. Re-derive the
  # address if the DIMMs are moved: the physical mapping changes with them.
  boot.kernelParams = ["memmap=4M\\$0x2AAB00000"];

  boot.loader.systemd-boot.configurationLimit = 5;
  myModules.bootloaders.systemdBoot.enable = false;
  myModules.bootloaders.grub = {
    enable = true;
    # Let os-prober generate the Windows chainloader entry. The explicit entry
    # duplicated it in the menu and was the one that failed to boot Windows.
    useOSProber = true;
    gfxmode = "3440x1440,auto";
  };
  boot.loader.grub2-theme = {
    enable = true;
    theme = "whitesur";
    icon = "whitesur";
    screen = "ultrawide2k";
  };

  networking.hostName = "ryzen-shine";
  myModules.hostIdentity = {
    emoticon = "☀️";
    tmux.background = "#2563eb";
  };

  environment.systemPackages = [
    inputs.nixpkgs-stable.legacyPackages.${pkgs.stdenv.hostPlatform.system}.inkscape
    pkgs.perf
    pkgs.wayvnc
  ];

  # Share the active Hyprland session over an SSH-only VNC endpoint. The
  # compositor imports WAYLAND_DISPLAY into the user manager during login, so
  # wayvnc can attach to the existing desktop without starting another session.
  systemd.user.services.wayvnc-share = {
    description = "Share the active Hyprland session over VNC";
    wantedBy = ["graphical-session.target"];
    partOf = ["graphical-session.target"];
    after = ["graphical-session.target"];
    serviceConfig = {
      ExecStart = "${pkgs.wayvnc}/bin/wayvnc --log-level=info 127.0.0.1 5900";
      Restart = "on-failure";
      RestartSec = 5;
    };
  };

  boot.initrd.systemd.enable = true;
  boot.plymouth = {
    enable = false;
  };

  hardware.enableRedistributableFirmware = true;

  boot.initrd.availableKernelModules = ["nvme" "xhci_pci" "ahci" "usb_storage" "usbhid" "sd_mod"];

  hardware.nvidia.modesetting.enable = true;

  # The open module otherwise leaves this GPU at a 256 MiB BAR1 aperture and
  # eventually exhausts its mapping VA space. When that happens, BAR1 VA runs
  # out under Hyprland plus Chrome on the 3440x1440 panel, nvidia-drm can no
  # longer map the framebuffer ("dmaAllocMapping_GM107: can't alloc VA space",
  # NV_ERR_NO_MEMORY out of kern_bus_gm107.c:3141 on pBar1VaInfo), the atomic
  # modeset fails -EAGAIN, the display engine wedges (Xid 16, Head 3), and the
  # GPU lands in NV_ERR_RESET_REQUIRED needing a reboot. Seen 2026-08-29
  # onward; 2026-09-07 wedged nine minutes into the boot.
  #
  # This parameter is INERT until "Above 4G Decoding" is enabled in the BIOS
  # (Advanced -> PCI Subsystem Settings, alongside Re-Size BAR Support; needs
  # CSM disabled under Boot). Without it the firmware hands the kernel no MMIO
  # window above 4 GiB -- every root bus window sits below 0x100000000 -- so
  # the driver's resize attempts from 8 GiB down to 512 MiB all fail with
  # "can't assign; no space" and BAR1 falls back to 256 MiB regardless.
  # Verify after changing the BIOS:
  #   journalctl -k -b | grep 'nvidia.*BAR 1'  # want "assigned", not "failed"
  #   lspci -v -s 0a:00.0 | grep prefetchable  # want an 8G prefetchable region
  hardware.nvidia.moduleParams.nvidia.NVreg_EnableResizableBar = 1;

  # 595.80 introduced a GSP-firmware regression on this RTX 3070 Ti (GA104):
  # random hard freezes with "GSP RM heartbeat timed out" / Xid 119 GSP RPC
  # timeouts (boots from 2026-06-08 onward). This host ran pinned to 595.71.05
  # until 2026-09-08; that pin also held the kernel back on 6.18, since the
  # driver predates Linux 7.2's DRM API changes. Moved to the new_feature
  # branch instead, which the old pin's comment named as the thing to try.
  # If the freezes return, that is the regression following us up a branch.
  hardware.nvidia.package = config.boot.kernelPackages.nvidiaPackages.new_feature;

  hardware.graphics.enable32Bit = true;

  boot.kernelModules = ["kvm-amd"];
  boot.extraModulePackages = [];

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/9bd06145-8151-4f7b-bcfe-f1809e0db1ea";
    fsType = "ext4";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/E1E1-909E";
    fsType = "vfat";
  };

  swapDevices = [
    {
      device = "/swapfile";
      size = 49152;
    }
  ];

  # nix.settings.maxJobs = lib.mkDefault 16;
  # High-DPI console
  console.font = lib.mkDefault "${pkgs.terminus_font}/share/consolefonts/ter-u28n.psf.gz";

  # services.xrdp.enable = true;
  # services.xrdp.defaultWindowManager = "startplasma-x11";
  # services.xrdp.openFirewall = true;

  system.autoUpgrade = {
    enable = true;
    dates = "daily";
  };

  system.stateVersion = "20.03";
  home-manager.sharedModules = [
    {
      home.stateVersion = "21.05";
      gtk.font.size = lib.mkForce 11;
      dconf.settings."org/gnome/desktop/interface".text-scaling-factor = 1.05;
    }
  ];

  # users.extraUsers.dean.home = "/shared/dean";
}
