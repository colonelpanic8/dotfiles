{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: let
  wayvncShare = pkgs.writeShellScript "wayvnc-share" ''
    set -euo pipefail

    state_dir="''${XDG_STATE_HOME:-$HOME/.local/state}/wayvnc"
    rsa_key="$state_dir/rsa_key.pem"
    if [ ! -s "$rsa_key" ]; then
      mkdir -p "$state_dir"
      ${pkgs.openssh}/bin/ssh-keygen -q -m pem -t rsa -N "" -f "$rsa_key"
    fi

    # Log in with the account password. Tailscale already encrypts the link,
    # so relax_encryption only adds Apple DH for macOS Screen Sharing.
    config_file="$XDG_RUNTIME_DIR/wayvnc-share.conf"
    cat > "$config_file" <<EOF
    enable_auth=true
    enable_pam=true
    relax_encryption=true
    rsa_private_key_file=$rsa_key
    EOF

    tailscale_ip="$(${config.services.tailscale.package}/bin/tailscale ip -4)"
    exec ${pkgs.wayvnc}/bin/wayvnc --config="$config_file" --log-level=info "$tailscale_ip" 5900
  '';
  # OpenZFS master, pinned: releases lag new kernels by weeks. Bump rev, hash
  # and kernelMaxSupportedMajorMinor (Linux-Maximum in the source's META) together.
  zfsDev = {
    kernelModuleAttribute = "zfs_unstable";
    kernelMinSupportedMajorMinor = "4.18";
    kernelMaxSupportedMajorMinor = "7.3";
    version = "2.4.99-unstable-2026-11-03";
    rev = "2d1eb9281af853d208e104915c1ed9f2f1a23e79";
    hash = "sha256-YL3qZ7nVF7wnMd67FDpqyFqmeltaqdmaVRYnOOAnVOM=";
    tests = {};
  };
  zfsGeneric = import "${pkgs.path}/pkgs/os-specific/linux/zfs/generic.nix";
  # nixpkgs' postPatch targets 2.4.x: master folded libshare into libzfs and
  # mounts snapshots in-kernel, leaving no mount-helper path to patch.
  zfsDevBuild = callPackage: args:
    (callPackage zfsGeneric args zfsDev).overrideAttrs (old: {
      postPatch =
        builtins.replaceStrings
        [
          "substituteInPlace ./module/os/linux/zfs/zfs_ctldir.c"
          "./lib/libshare/os/linux/nfs.c"
          "./lib/libshare/smb.h"
        ]
        [
          "true"
          "./lib/libzfs/os/linux/libzfs_share_nfs.c"
          "./lib/libzfs/libzfs_share.h"
        ]
        old.postPatch;
    });

  # Newest kernel the pinned ZFS supports, so a new kernel release waits for a
  # ZFS bump instead of breaking the build.
  zfsKernelPackages = let
    versionOf = name: lib.concatStringsSep "." (lib.drop 1 (lib.splitString "_" name));
    supported =
      lib.filter (
        name:
          builtins.match "linux_[0-9]+_[0-9]+" name
          != null
          && lib.versionAtLeast zfsDev.kernelMaxSupportedMajorMinor (versionOf name)
      )
      (builtins.attrNames pkgs.linuxKernel.packages);
  in
    pkgs.linuxKernel.packages.${lib.last (lib.sort (a: b: lib.versionOlder (versionOf a) (versionOf b)) supported)};
in {
  imports = [
    ../configuration.nix
    ../vaultwarden-jimi.nix
    ../simlock-jimi.nix
    ../rocket-sense-jimi.nix
  ];

  myModules.tailscale.enrollment = "ivan";

  services.macos-ventura = {
    enable = false;
    openFirewall = true;
    vncListenAddr = "0.0.0.0";
  };

  features.full.enable = true;
  nix.gc = {
    automatic = true;
    dates = "daily";
    options = "--delete-older-than 7d";
  };
  nix.settings.auto-optimise-store = true;
  myModules.taffybar.aiUsageMode = "both";
  myModules.cache-server = {
    enable = true;
    port = 3090;
  };
  myModules.gitea-runner.enable = false;
  myModules.vscode.enable = true;
  myModules.games.enable = lib.mkForce true;
  myUsers.kat.apps.enable = true;
  myModules.ntfy.enable = true;
  myModules.nvidia.enable = true;
  myModules.hyprland.ultrawideRefreshRate = 99.98;
  environment.systemPackages = [pkgs.wayvnc];
  security.pam.services.wayvnc = {};

  # Share the active Hyprland desktop only on this host's Tailscale address.
  systemd.user.services.wayvnc-share = {
    description = "Share the active Hyprland session over Tailscale VNC";
    wantedBy = ["graphical-session.target"];
    partOf = ["graphical-session.target"];
    after = ["graphical-session.target"];
    serviceConfig = {
      ExecStart = "${wayvncShare}";
      Restart = "on-failure";
      RestartSec = 5;
    };
  };

  myModules.railbird-k3s = {
    enable = false;
    serverAddr = "https://dev.railbird.ai:6443";
  };
  services.k3s.enable = lib.mkForce false;

  networking.hostName = "jimi-hendnix";
  myUsers.imalison.level = "full";
  networking.networkmanager.wifi.powersave = false;
  myModules.hostIdentity = {
    emoticon = "🐩";
    tmux.background = "#db2777";
  };

  hardware.enableRedistributableFirmware = true;
  boot.kernelPackages = zfsKernelPackages.extend (self: super: {
    zfs_unstable = zfsDevBuild self.callPackage {
      configFile = "kernel";
      inherit (self) kernel;
    };
    # v4l2loopback 0.15.4 installs depmod indexes that collide with the kernel's.
    v4l2loopback = super.v4l2loopback.overrideAttrs (old: {
      postInstall =
        (old.postInstall or "")
        + ''
          rm -f $out/lib/modules/*/modules.*
        '';
    });
  });
  boot.initrd.availableKernelModules = ["xhci_pci" "ahci" "nvme" "usbhid" "usb_storage" "sd_mod"];
  boot.initrd.kernelModules = [];
  boot.kernelModules = ["kvm-intel"];
  boot.extraModulePackages = [];
  boot.loader.systemd-boot.enable = true;

  hardware.nvidia.modesetting.enable = true;
  hardware.nvidia.moduleParams.nvidia.NVreg_EnableResizableBar = 1;

  # This also enables v4l2loopback
  programs.droidcam.enable = true;

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/31030df2-0431-4309-88f0-5110205397a3";
    fsType = "ext4";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/30D8-975B";
    fsType = "vfat";
  };

  # Data-only ZFS pools; boot never needs them. tank: Intel 660p (nvme1n1p2).
  # sata: unredundant stripe of the two Samsung SATA SSDs, for losable data.
  boot.supportedFilesystems.zfs = true;
  boot.zfs.extraPools = ["tank" "sata"];
  boot.zfs.package = zfsDevBuild pkgs.callPackage {configFile = "user";};
  networking.hostId = "02ad6f98";
  boot.kernelParams = ["zfs.zfs_arc_max=6442450944"];
  services.zfs.autoScrub.enable = true;

  # Prefer the SN770 over the QLC Intel 660p.
  swapDevices = [
    {
      device = "/dev/disk/by-uuid/598e9aa1-4940-4410-a2fa-3dfd8b7d2c0d";
      priority = 10;
    }
    {
      device = "/dev/disk/by-uuid/c0dcff59-8072-48fb-b242-a7a1797e4b48";
      priority = 1;
    }
  ];

  # Absorb swap traffic in RAM before touching either disk.
  zramSwap = {
    enable = true;
    algorithm = "zstd";
    priority = 100;
  };

  networking.useDHCP = lib.mkDefault true;
  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

  services.xrdp.enable = true;
  services.xrdp.defaultWindowManager = "startplasma-x11";
  services.xrdp.openFirewall = true;
  systemd.services.xrdp.serviceConfig.RestrictNetworkInterfaces = "lo tailscale0";

  system.autoUpgrade = {
    enable = true;
    dates = "hourly";
  };

  home-manager.sharedModules = [
    {
      home.stateVersion = "23.11";
    }
  ];

  system.stateVersion = "23.11";
}
