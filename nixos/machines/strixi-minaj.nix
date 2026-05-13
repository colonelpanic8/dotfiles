{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: let
  builtInAudioCard = "alsa_card.pci-0000_00_1f.3";
  builtInAudioDuplexProfile = "output:analog-stereo+input:analog-stereo";
  setBuiltInAudioDuplexProfile = pkgs.writeShellScript "set-built-in-audio-duplex-profile" ''
    attempts=0
    while [ "$attempts" -lt 20 ]; do
      if ${pkgs.pulseaudio}/bin/pactl set-card-profile ${builtInAudioCard} ${builtInAudioDuplexProfile}; then
        exit 0
      fi
      attempts=$((attempts + 1))
      sleep 0.25
    done

    ${pkgs.pulseaudio}/bin/pactl set-card-profile ${builtInAudioCard} ${builtInAudioDuplexProfile}
  '';
in {
  imports = [
    ../configuration.nix
    inputs.grub2-themes.nixosModules.default
    inputs.nixos-hardware.nixosModules.asus-rog-strix-g834jzr
  ];

  myModules.base.enable = true;
  myModules.desktop.enable = true;
  myModules.xmonad.enable = true;
  myModules.riverXmonad.enable = true;
  myModules.extra.enable = false;
  myModules.code.enable = true;
  myModules.games.enable = false;
  myModules.syncthing.enable = true;
  myModules.fonts.enable = true;
  myModules.gitea-runner.enable = false;
  myModules.nvidia.enable = true;
  myModules.electron.enable = true;
  myModules.wyoming.enable = false;
  myModules.tts.enable = true;
  myModules."keepbook-sync".enable = true;

  hardware.enableRedistributableFirmware = true;

  services.pipewire.wireplumber.extraConfig."51-strixi-built-in-audio-duplex" = {
    "monitor.alsa.rules" = [
      {
        matches = [
          {
            "device.name" = builtInAudioCard;
          }
        ];
        actions.update-props = {
          "device.profile" = builtInAudioDuplexProfile;
        };
      }
    ];
  };

  systemd.user.services.set-built-in-audio-duplex-profile = {
    description = "Enable the built-in audio input profile";
    wantedBy = ["default.target"];
    wants = ["pipewire-pulse.service" "wireplumber.service"];
    after = ["pipewire-pulse.service" "wireplumber.service"];
    serviceConfig = {
      Type = "oneshot";
      ExecStart = setBuiltInAudioDuplexProfile;
    };
  };

  # nixpkgs.config.cudaSupport = true;

  myModules.bootloaders.systemdBoot.enable = false;
  myModules.bootloaders.grub = {
    enable = true;
    configurationLimit = 5;
    gfxmode = "2560x1600,auto";
  };
  boot.loader.grub2-theme = {
    enable = true;
    theme = "whitesur";
    icon = "whitesur";
    screen = "2k";
    customResolution = "2560x1600";
  };

  environment.systemPackages = with pkgs; [
    android-studio
    gimp
    inkscape
  ];

  services.synergy.server = {
    enable = true;
    autoStart = true;
    configFile = ../../dotfiles/synergy.conf;
  };

  services.matter-server = {
    enable = false;
    logLevel = "debug";
    extraArgs = ["--bluetooth-adapter=0" "--enable-test-net-dcl"];
  };

  programs.virt-manager.enable = true;
  virtualisation.libvirtd.enable = true;
  virtualisation.spiceUSBRedirection.enable = true;
  systemd.services.virt-secret-init-encryption.serviceConfig.ExecStart = lib.mkForce [
    ""
    "${pkgs.runtimeShell} -c 'umask 0077 && (dd if=/dev/random status=none bs=32 count=1 | systemd-creds encrypt --name=secrets-encryption-key - /var/lib/libvirt/secrets/secrets-encryption-key)'"
  ];

  services.xserver.dpi = 96;
  # linuxPackages_latest (6.19) currently fails to build nvidia-open.
  # Keep this host on the default kernel packages until the driver catches up.
  boot.kernelPackages = pkgs.linuxPackages;
  # See https://github.com/NixOS/nixpkgs/issues/467814 for why this was needed
  hardware.nvidia.package = config.boot.kernelPackages.nvidiaPackages.beta;
  boot.initrd.availableKernelModules = ["vmd" "xhci_pci" "thunderbolt" "nvme" "usbhid" "usb_storage" "sd_mod"];
  boot.initrd.kernelModules = ["nvidia" "nvidia_drm" "nvidia_uvm" "nvidia_modeset"];
  boot.kernelModules = ["kvm-intel"];
  boot.extraModulePackages = [];
  hardware.nvidia.powerManagement.enable = true;
  # This laptop has a hardware MUX, so prefer dGPU-only mode instead of
  # PRIME sync hybrid mode to keep the compositor and displays on NVIDIA.
  hardware.nvidia.prime.offload.enable = lib.mkForce false;
  hardware.nvidia.prime.sync.enable = lib.mkForce false;
  services.asusd.enable = true;
  services.supergfxd.settings = {
    mode = "AsusMuxDgpu";
  };
  services.power-profiles-daemon.enable = false;
  services.tlp.enable = false;

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/fc06a54c-cc45-423a-914b-8dfcb5939106";
    fsType = "ext4";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/B28A-829A";
    fsType = "vfat";
  };

  swapDevices = [
    {device = "/dev/disk/by-uuid/27f277a0-b552-43a0-904d-625e48922bb9";}
    {
      device = "/swapfile";
      size = 16384;
    } # size is in MiB (adds 16 GiB)
  ];

  networking.hostName = "strixi-minaj";
  myModules.hostIdentity = {
    emoticon = "👩🏿";
    tmux.background = "#ea580c";
  };

  powerManagement.cpuFreqGovernor = lib.mkDefault "performance";
  hardware.cpu.intel.updateMicrocode = lib.mkDefault true;

  home-manager.sharedModules = [
    {
      services.kanshi.settings = lib.mkAfter [
        {
          profile.name = "internal-only";
          profile.outputs = [
            {
              criteria = "eDP-1";
              status = "enable";
              mode = "2560x1600@240Hz";
              position = "0,0";
              scale = 1.25;
            }
            {
              criteria = "Microstep MPG341CX OLED Unknown";
              status = "disable";
            }
          ];
        }
        {
          profile.name = "internal-panel-only";
          profile.outputs = [
            {
              criteria = "eDP-1";
              status = "enable";
              mode = "2560x1600@240Hz";
              position = "0,0";
              scale = 1.25;
            }
          ];
        }
      ];

      home.stateVersion = "23.05";
    }
  ];

  system.autoUpgrade = {
    enable = true;
    dates = "hourly";
  };

  system.stateVersion = "23.05";
}
