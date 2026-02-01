{ config, lib, pkgs, forEachUser, inputs, orgAgendaApiContainer ? null, orgAgendaApiImageName ? "org-agenda-api", ... }:
{
  imports = [
    ../configuration.nix
    inputs.agenix.nixosModules.default
  ];

  networking.hostName = "railbird-sf";

  # org-agenda-api hosting with nginx + Let's Encrypt
  # Separate secrets for org-agenda-api: auth password (env format) and SSH key (raw file)
  age.secrets.org-api-auth-password = {
    file = ../secrets/org-api-auth-password.age;
  };
  age.secrets.org-api-ssh-key = {
    file = ../secrets/org-api-ssh-key.age;
    mode = "0400";  # Restrictive permissions for SSH key
  };

  services.org-agenda-api-host = {
    enable = true;
    domain = "rbsf.tplinkdns.com";
    containerImage = orgAgendaApiImageName;
    containerImageFile = orgAgendaApiContainer;
    secretsFile = config.age.secrets.org-api-auth-password.path;
    sshKeyFile = config.age.secrets.org-api-ssh-key.path;
  };

  hardware.enableRedistributableFirmware = true;
  boot.initrd.availableKernelModules = [ "nvme" "xhci_pci" "ahci" "usbhid" "usb_storage" "sd_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-amd" ];
  boot.extraModulePackages = [ ];
  boot.loader.systemd-boot.enable = true;
  myModules.postgres.enable = true;
  features.full.enable = true;

  services.k3s.role = "agent";
  services.k3s.extraFlags = lib.mkForce ["--node-label nixos-nvidia-cdi=enabled"];

  hardware.nvidia = {
    powerManagement.enable = false;
    # Fine-grained power management. Turns off GPU when not in use.
    # Experimental and only works on modern Nvidia GPUs (Turing or newer).
    powerManagement.finegrained = false;

    # Enable the Nvidia settings menu,
    # accessible via `nvidia-settings`.
    nvidiaSettings = true;
  };

  myModules.base.enable = true;
  myModules.desktop.enable = true;
  myModules.code.enable = true;
  myModules.syncthing.enable = true;
  myModules.fonts.enable = true;
  myModules.plasma.enable = true;
  myModules.nvidia.enable = true;
  myModules.gitea-runner.enable = true;
  myModules.railbird-k3s = {
    enable = false;
    serverAddr = "https://dev.railbird.ai:6443";
  };

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/a317d456-6f84-41ee-a149-8e466e414aae";
    fsType = "ext4";
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/B875-39D4";
      fsType = "vfat";
      };

  swapDevices =
    [ { device = "/dev/disk/by-uuid/129345f3-e1e1-4d45-9db9-643160c6d564"; }
    ];

  environment.systemPackages = with pkgs; [
    android-studio
  ];

  networking.useDHCP = lib.mkDefault true;
  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
  hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

  home-manager.users = forEachUser {
     home.stateVersion = "23.11";
  };

  system.stateVersion = "23.11";
}
