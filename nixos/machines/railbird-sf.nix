{
  config,
  lib,
  pkgs,
  inputs,
  orgAgendaApiContainer ? null,
  orgAgendaApiImageName ? "localhost/org-agenda-api:colonelpanic-dbb1cb8-030a673",
  ...
}: {
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
  # DuckDNS token for the rocket-sense.duckdns.org dynamic-DNS updater below.
  age.secrets.duckdns-token = {
    file = ../secrets/duckdns-token.railbird-sf.age;
    mode = "0400";
  };

  services.org-agenda-api-host = {
    enable = true;
    domain = "rocket-sense.duckdns.org";
    extraDomains = ["org-agenda-api.duckdns.org"];
    containerImage = orgAgendaApiImageName;
    containerImageFile = orgAgendaApiContainer;
    secretsFile = config.age.secrets.org-api-auth-password.path;
    sshKeyFile = config.age.secrets.org-api-ssh-key.path;
  };

  hardware.enableRedistributableFirmware = true;
  boot.initrd.availableKernelModules = ["nvme" "xhci_pci" "ahci" "usbhid" "usb_storage" "sd_mod"];
  boot.initrd.kernelModules = [];
  boot.kernelModules = ["kvm-amd"];
  boot.extraModulePackages = [];
  boot.loader.systemd-boot.enable = true;
  myModules.postgres.enable = true;
  features.full.enable = true;

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
  myModules.syncthing.enable = true;
  myModules.fonts.enable = true;
  myModules.plasma.enable = true;
  myModules.nvidia.enable = true;
  myModules.gitea-runner.enable = true;
  myModules.railbird-k3s = {
    enable = true;
    serverAddr = "";
  };
  myModules."keepbook-sync".enable = true;
  myModules.remote-hyprland.enable = true;

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
      alias = pkgs.writeText "rocket-sense-privacy.html" ''
        <!doctype html>
        <html lang="en">
          <head>
            <meta charset="utf-8">
            <meta name="viewport" content="width=device-width, initial-scale=1">
            <title>Rocket Sense Privacy Policy</title>
            <style>
              body {
                color: #171717;
                font-family: system-ui, -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif;
                line-height: 1.55;
                margin: 0;
                padding: 48px 24px;
              }
              main {
                max-width: 760px;
                margin: 0 auto;
              }
              h1, h2 {
                line-height: 1.2;
              }
              h1 {
                font-size: 2rem;
              }
              h2 {
                font-size: 1.2rem;
                margin-top: 2rem;
              }
            </style>
          </head>
          <body>
            <main>
              <h1>Rocket Sense Privacy Policy</h1>
              <p>Last updated: June 18, 2026</p>

              <p>Rocket Sense is a Rocket League replay analysis service operated by Ivan Malison. This policy explains what information the service uses when you sign in and upload or view replay data.</p>

              <h2>Information We Collect</h2>
              <p>When you sign in with Epic Games, Rocket Sense receives the Epic account identifiers and profile fields that you approve during the Epic login flow, such as account ID and display name. When you use the service, Rocket Sense may store replay files, replay metadata, player identifiers, derived gameplay statistics, and basic account/session records needed to operate the site.</p>

              <h2>How We Use Information</h2>
              <p>Rocket Sense uses this information to authenticate users, associate uploads with accounts, process replay files, display replay-derived statistics, maintain site reliability, and prevent abuse.</p>

              <h2>Sharing</h2>
              <p>Rocket Sense does not sell personal information. Information may be processed by infrastructure providers used to host and operate the service, and may be disclosed if required by law or necessary to protect the service and its users.</p>

              <h2>Retention</h2>
              <p>Rocket Sense keeps account, replay, and derived analysis data for as long as needed to provide the service, debug issues, maintain security, or comply with legal obligations.</p>

              <h2>Contact</h2>
              <p>For privacy questions or deletion requests, contact Ivan Malison at IvanMalison@gmail.com.</p>
            </main>
          </body>
        </html>
      '';
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

  services.nginx.virtualHosts."rbsf.tplinkdns.com" = {
    useACMEHost = "rocket-sense.duckdns.org";
    forceSSL = true;
    globalRedirect = "rocket-sense.duckdns.org";
  };

  security.acme.certs."rocket-sense.duckdns.org".extraDomainNames = ["rbsf.tplinkdns.com"];

  # Dynamic-DNS updater: keep rocket-sense.duckdns.org pointed at this node's
  # current public IP. The residential WAN IP changes (e.g. after an ISP
  # outage/failover), and without this the public hostname goes stale and the
  # site becomes unreachable until manually re-pointed. Leaving ip= blank tells
  # DuckDNS to detect the source IP of this request, so it always reflects
  # whatever connection the node is currently egressing through.
  systemd.services.duckdns = {
    description = "DuckDNS updater for rocket-sense.duckdns.org";
    after = ["network-online.target"];
    wants = ["network-online.target"];
    serviceConfig = {
      Type = "oneshot";
      DynamicUser = true;
      LoadCredential = ["token:${config.age.secrets.duckdns-token.path}"];
    };
    script = ''
      token="$(cat "$CREDENTIALS_DIRECTORY/token")"
      resp="$(${pkgs.curl}/bin/curl -fsS --retry 3 --max-time 30 \
        "https://www.duckdns.org/update?domains=rocket-sense&token=$token&ip=")"
      echo "DuckDNS response: $resp"
      [ "$resp" = "OK" ] || { echo "DuckDNS update failed"; exit 1; }
    '';
  };

  systemd.timers.duckdns = {
    description = "Periodic DuckDNS update for rocket-sense.duckdns.org";
    wantedBy = ["timers.target"];
    timerConfig = {
      OnBootSec = "1min";
      OnUnitActiveSec = "5min";
      Persistent = true;
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
