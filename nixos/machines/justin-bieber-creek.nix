{ config, lib, pkgs, forEachUser, ... }:

{
  imports = [
    ../configuration.nix
  ];

  services.synergy.server = {
    enable = true;
  };

  myModules.fonts.enable = true;
  myModules.base.enable = true;
  myModules.desktop.enable = true;
  myModules.xmonad.enable = true;

  networking.enableIPv6 = true;

  services.synergy.client = {
    enable = true;
    serverAddress = "strixi-minaj.local:24800";
  };

  boot.kernel.sysctl = {
    # For all interfaces (e.g. if you want to accept RA on all):
    "net.ipv6.conf.all.accept_ra" = lib.mkForce "1";
    "net.ipv6.conf.all.accept_ra_rt_info_max_plen" = lib.mkForce "64";
    "net.ipv6.conf.default.accept_ra" = lib.mkForce "1";
    "net.ipv6.conf.default.accept_ra_rt_info_max_plen" = lib.mkForce "64";
    "net.ipv6.conf.wlo1.accept_ra" = lib.mkForce "1";
    "net.ipv6.conf.wlo1.accept_ra_rt_info_max_plen" = lib.mkForce "64";

    # Ensure forwarding is off on all interfaces unless needed
    "net.ipv6.conf.all.forwarding" = lib.mkForce "0";
  };

  systemd.services.otbr-agent = {
    wants = [ "network-online.target" ];
    after = [ "network-online.target" ];
  };

  services.openthread-border-router = {
    enable = true;
    backboneInterface = "wpan0";
    logLevel = "debug";
    radio =  {
      device = "/dev/serial/by-id/usb-Nabu_Casa_Home_Assistant_Connect_ZBT-1_0cd053abfa38ef119c66e1d154516304-if00-port0";
      baudRate = 460800;
      flowControl = true;
    };
    web = {
      listenPort = 8087;
    };
    rest = {
      listenPort = 8089;
    };
  };

  services.matter-server = {
    enable = true;
    logLevel = "debug";
    extraArgs = let cert-dir = pkgs.fetchFromGitHub {
      repo = "connectedhomeip";
      owner = "project-chip";
      rev = "6e8676be6142bb541fa68048c77f2fc56a21c7b1";
      hash = "sha256-QwPKn2R4mflTKMyr1k4xF04t0PJIlzNCOdXEiQwX5wk=";
    }; in
    ["--bluetooth-adapter=0" "--paa-root-cert-dir=${cert-dir}/credentials/production/paa-root-certs" "--enable-test-net-dcl"];
  };

  age.secrets.google-service-account = {
    file = ../secrets/google-assistant-integration-service-key.age;
    owner = "hass";
  };

  services.home-assistant = {
    enable = true;
    extraComponents = [
      "anthropic"
      "cast"
      "dlna_dmr"
      "esphome"
      "google_assistant"
      "homeassistant_hardware"
      "homeassistant_sky_connect"
      "homekit_controller"
      "ibeacon"
      "isal"
      "kef"
      "kegtron"
      "matter"
      "met"
      "opensky"
      "otbr"
      "radio_browser"
      "roomba"
      "samsungtv"
      "thread"
      "webostv"
      "yale"
    ];
    extraPackages = python3Packages: with python3Packages; [
      numpy
      python-matter-server
      universal-silabs-flasher
    ];
    config = {
      http = {
        use_x_forwarded_for = true;
        trusted_proxies = ["0.0.0.0" "127.0.0.1" "::1" "192.168.50.1"];
      };
      google_assistant = {
        project_id = "canyon-run-b104-home-assistant";
        service_account = "!include ${config.age.secrets.google-service-account.path}";
        report_state = true;
        exposed_domains = ["switch" "light"];
      };
      default_config = {};
    };
  };

  security.acme = {
    acceptTerms = true;
    defaults.email = "IvanMalison@gmail.com";
  };

  services.nginx = {
    enable = true;
    recommendedProxySettings = true;
    recommendedGzipSettings = true;
    recommendedTlsSettings = true;

    virtualHosts = {
      "homeassistant.canyonrunb104.duckdns.org" = {
        enableACME = true;
        addSSL = true;
        locations."/" = {
          proxyPass = "http://localhost:8123";
          extraConfig = ''
            proxy_set_header Upgrade $http_upgrade;
            proxy_set_header Connection "upgrade";
          '';
        };
      };
      "ha.canyonrunb104.duckdns.org" = {
        enableACME = true;
        addSSL = true;
        locations."/" = {
          proxyPass = "http://localhost:8123";
          extraConfig = ''
            proxy_set_header Upgrade $http_upgrade;
            proxy_set_header Connection "upgrade";
          '';
        };
      };
    };
  };

  boot.loader.systemd-boot.configurationLimit = 3;

  boot.kernelPackages = pkgs.linuxPackages_latest;

  boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "usbhid" "usb_storage" "sd_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];
  boot.loader.systemd-boot.enable = true;

  # Add Intel Wi-Fi firmware
  hardware.enableRedistributableFirmware = true;
  hardware.enableAllFirmware = true;

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/453d28a1-26f2-4b25-ac72-c6d301fd0bb8";
      fsType = "ext4";
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/184E-E5E8";
      fsType = "vfat";
    };

  swapDevices = [ ];

  networking.hostName = "justin-bieber-creek";

  networking.useDHCP = false;
  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

  system.stateVersion = "23.05";

  home-manager.users = forEachUser {
    home.stateVersion = "23.05";
  };
}
