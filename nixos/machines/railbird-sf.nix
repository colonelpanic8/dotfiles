{
  config,
  lib,
  pkgs,
  inputs,
  orgAgendaApiContainer ? null,
  orgAgendaApiImageName ? "localhost/org-agenda-api:colonelpanic-3a33f61-6a4ec8d",
  ...
}: let
  # Mova's web client, served beside the API on the same hosts. A static export
  # bakes its asset URLs at build time, so the package and nginx have to agree
  # on the prefix.
  movaWebPath = "/app";
  movaWeb = inputs.mova.packages.${pkgs.stdenv.hostPlatform.system}.web.override {baseUrl = movaWebPath;};
in {
  imports = [
    ../configuration.nix
    inputs.agenix.nixosModules.default
  ];

  networking.hostName = "railbird-sf";
  myModules.hostIdentity = {
    emoticon = "🐦";
    tmux.background = "#0891b2";
  };

  # Allow nginx to serve content synced into /var/lib/syncthing/* (owned by syncthing:syncthing, 2770 perms).
  users.users.nginx.extraGroups = ["syncthing"];

  # org-agenda-api hosting with nginx + Let's Encrypt
  # Separate secrets for org-agenda-api: auth password (env format) and SSH key (raw file)
  age.secrets.org-api-auth-password = {
    file = ../secrets/org-api-auth-password.age;
  };
  age.secrets.org-api-ssh-key = {
    file = ../secrets/org-api-ssh-key.age;
    mode = "0400"; # Restrictive permissions for SSH key
  };

  services.org-agenda-api-host = {
    enable = true;
    domain = "rocket-sense.duckdns.org";
    extraDomains = ["org-agenda-api.duckdns.org"];
    containerImage = orgAgendaApiImageName;
    containerImageFile = orgAgendaApiContainer;
    secretsFile = config.age.secrets.org-api-auth-password.path;
    sshKeyFile = config.age.secrets.org-api-ssh-key.path;
    webClient = movaWeb;
    webClientPath = movaWebPath;
  };

  hardware.enableRedistributableFirmware = true;
  boot.initrd.availableKernelModules = ["nvme" "xhci_pci" "ahci" "usbhid" "usb_storage" "sd_mod"];
  boot.initrd.kernelModules = [];
  boot.kernelModules = ["kvm-amd"];
  boot.extraModulePackages = [];
  boot.loader.systemd-boot.enable = true;
  myModules.postgres.enable = true;
  features.full.enable = true;

  # CI on this box grows /nix unboundedly (per-commit `nix develop
  # '.?submodules=1'` source copies + railbird-full CUDA image builds); the
  # store had 20k dead paths / 40G+ reclaimable when this was added. Daily GC
  # keeps the baseline down; min-free makes nix GC mid-build when free space
  # drops below 50G instead of running into ENOSPC. (The CI postgres DiskFull
  # failures were a separate issue — the runner's private 3.2G tmpfs /tmp;
  # see gitea-runner.nix TMPDIR.)
  nix.gc = {
    automatic = true;
    dates = "daily";
    options = "--delete-older-than 7d";
  };
  nix.settings = {
    min-free = 50 * 1024 * 1024 * 1024; # trigger GC below 50G free
    max-free = 150 * 1024 * 1024 * 1024; # GC until 150G free
  };

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
  myModules.claudeRemoteControl.enable = true;
  # Dean only lingered here to run a headless T3 Code server.
  users.users.dean.linger = false;
  myModules.syncthing.enable = true;
  myModules.fonts.enable = true;
  myModules.plasma.enable = true;
  myModules.nvidia.enable = true;
  myModules.githubAgentRunner.enable = true;
  myModules.gitea-runner.enable = true;
  myModules.railbird-k3s = {
    enable = true;
    serverAddr = "";
  };
  myModules."keepbook-sync".enable = true;
  # Keeps the claude/codex remote-control user services running without a login.
  users.users.imalison.linger = true;

  # GL.iNet Comet Pro KVM on the LAN only has a self-signed cert; front it with a tailnet cert.
  systemd.services.glkvm-serve = {
    description = "Tailscale HTTPS for the GL.iNet KVM";
    after = ["tailscaled.service"];
    wants = ["tailscaled.service"];
    wantedBy = ["multi-user.target"];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      TimeoutStartSec = "60s";
      ExecStartPre = "${config.services.tailscale.package}/bin/tailscale wait --timeout=45s";
      ExecStart = "${pkgs.util-linux}/bin/flock /run/tailscale-serve.lock ${config.services.tailscale.package}/bin/tailscale serve --bg --https=8443 --set-path=/ https+insecure://192.168.0.6";
      Restart = "on-failure";
      RestartSec = "5s";
    };
  };

  # Mirror the old biskcomp "Syncthing hosting" pattern: serve the synced railbird tree over HTTPS with autoindex.
  services.nginx.virtualHosts."syncthing.railbird.ai" = {
    enableACME = true;
    forceSSL = true;
    root = "/var/lib/syncthing/railbird";
    locations."/" = {
      extraConfig = ''
        autoindex on;
      '';
    };
  };

  services.nginx.virtualHosts."docs.railbird.ai" = {
    enableACME = true;
    forceSSL = true;
    root = "/var/lib/syncthing/railbird/docs";
    locations."/" = {
      extraConfig = ''
        autoindex on;
      '';
    };
  };

  services.nginx.virtualHosts."rocket-sense.duckdns.org" = {
    enableACME = true;
    forceSSL = true;
    locations."= /privacy" = {
      alias = ../rocket-sense-privacy.html;
      extraConfig = ''
        default_type text/html;
      '';
    };
    locations."/" = {
      proxyPass = "http://127.0.0.1:30080";
      proxyWebsockets = true;
      extraConfig = ''
        client_max_body_size 200m;
        proxy_read_timeout 300s;
        proxy_connect_timeout 75s;
      '';
    };
  };

  # Open the standard Syncthing sync/discovery ports on the host firewall.
  # Note: you may still need router/NAT port-forwards for inbound access from the internet.
  services.syncthing.openDefaultPorts = true;

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/a317d456-6f84-41ee-a149-8e466e414aae";
    fsType = "ext4";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/B875-39D4";
    fsType = "vfat";
  };

  # Bulk data disk. Automounting keeps boot independent of the drive, while
  # the short timeouts make access fail quickly if the disk is absent or sick.
  fileSystems."/mnt/hard-drive" = {
    device = "/dev/disk/by-uuid/89856ef2-a14a-4590-827e-674572125c64";
    fsType = "ext4";
    options = [
      "nofail"
      "x-systemd.automount"
      "x-systemd.device-timeout=1s"
      "x-systemd.mount-timeout=5s"
    ];
  };

  swapDevices = [
    {device = "/dev/disk/by-uuid/129345f3-e1e1-4d45-9db9-643160c6d564";}
  ];

  environment.systemPackages = with pkgs; [
    android-studio
  ];

  networking.useDHCP = lib.mkDefault true;
  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
  hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

  home-manager.sharedModules = [
    {
      home.stateVersion = "23.11";
    }
  ];

  system.autoUpgrade = {
    enable = true;
    dates = "hourly";
  };

  system.stateVersion = "23.11";
}
