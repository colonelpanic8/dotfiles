{ config, lib, pkgs, forEachUser, ... }:

{
  imports = [
    ../configuration.nix
  ];

  myModules.railbird-k3s = {
    enable = true;
    serverAddr = "https://ryzen-shine.local:6443";
  };
  myModules.base.enable = true;
  myModules.desktop.enable = true;
  myModules.xmonad.enable = true;
  myModules.extra.enable = false;
  myModules.code.enable = true;
  myModules.games.enable = false;
  myModules.syncthing.enable = true;
  myModules.fonts.enable = true;
  myModules.gitea-runner.enable = true;
  myModules.postgres.enable = true;
  myModules.gitea.enable = true;

  age.secrets.vaultwarden-environment-file = {
    file = ../secrets/vaultwarden-environment-file.age;
    owner = "vaultwarden";
  };

  services.vaultwarden = {
    enable = true;
    backupDir = "/var/backup/vaultwarden";
    environmentFile = config.age.secrets.vaultwarden-environment-file.path;
    config = {
      ROCKET_ADDRESS = "::1";
      ROCKET_PORT = 8222;
    };
  };

  services.nginx = {
    enable = true;
    recommendedProxySettings = true;
    recommendedGzipSettings = true;
    recommendedTlsSettings = true;
    virtualHosts = {
      "vaultwarden.railbird.ai" = {
        enableACME = true;
        forceSSL = true;
        locations."/" = {
          proxyPass = "http://[::1]:8222";
        };
      };
      "syncthing.railbird.ai" = {
        enableACME = true;
        forceSSL = true;
        root = "/var/lib/syncthing/railbird";
        locations."/" = {
          extraConfig = ''
            autoindex on;
          '';
        };
      };
      "docs.railbird.ai" = {
        enableACME = true;
        forceSSL = true;
        root = "/var/lib/syncthing/railbird/docs";
        locations."/" = {
          extraConfig = ''
            autoindex on;
          '';
        };
      };
    };
  };

  security.acme = {
    acceptTerms = true;
    defaults.email = "IvanMalison@gmail.com";
  };

  hardware.enableRedistributableFirmware = true;
  myModules.nvidia.enable = true;

  boot.initrd.availableKernelModules = [ "xhci_pci" "nvme" "usb_storage" "usbhid" "sd_mod" ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  hardware.nvidia.modesetting.enable = true;

  services.xserver = {
    videoDrivers = [ "nvidia" ];
  };

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/30583504-9530-4095-a556-da1209ef9b63";
      fsType = "ext4";
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/CE95-E46C";
      fsType = "vfat";
    };

  swapDevices = [
    { device = "/dev/disk/by-uuid/598e9aa1-4940-4410-a2fa-3dfd8b7d2c0d"; }
  ];

  home-manager.users = forEachUser {
    home.stateVersion = "23.11";
  };

  networking.hostName = "jimi-hendnix";

  system.stateVersion = "23.05";
}
