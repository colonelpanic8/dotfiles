{
  pkgs,
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.myModules.railbird-k3s;
  mount-path = "/var/lib/railbird/bucket";
  bucket-name = "railbird-dev-videos";
  plugins-path = pkgs.buildEnv {
    name = "combined-cni-plugins";
    paths = [
      pkgs.cni-plugins
      pkgs.calico-cni-plugin
      pkgs.calico-kube-controllers
      pkgs.cni-plugin-flannel
    ];
  };
in {
  options = {
    myModules.railbird-k3s = {
      enable = mkEnableOption "railbird k3s";
      serverAddr = mkOption {
        type = lib.types.str;
        default = "";
      };
      extraFlags = mkOption {
        type = lib.types.listOf lib.types.str;
        default = [];
      };
    };
  };
  config = mkIf cfg.enable {
    age.secrets."1896Folsom-k3s-token.age".file = ./secrets/1896Folsom-k3s-token.age;
    age.secrets."k3s-registry.yaml.age".file = ./secrets/k3s-registry.yaml.age;
    age.secrets.api-service-key = {
      file = ./secrets/api_service_account_key.json.age;
      owner = "railbird";
      group = "users";
    };
    environment.etc."rancher/k3s/registries.yaml".source = config.age.secrets."k3s-registry.yaml.age".path;
    services.dockerRegistry = {
      enable = true;
      listenAddress = "0.0.0.0";
      port = 5279;
      enableDelete = true;
      enableGarbageCollect = true;
    };

    virtualisation.containerd = {
      enable = true;
      settings = {
        plugins."io.containerd.cri.v1.runtime" = {
          enable_cdi = true;
          cdi_spec_dirs = [ "/var/run/cdi" ];
        };
        plugins."io.containerd.grpc.v1.cri" = {
          enable_cdi = true;
          cdi_spec_dirs = [ "/var/run/cdi" ];
          cni.bin_dir = "${plugins-path}/bin";
        };
      };
    };

    hardware.nvidia-container-toolkit.enable = true;
    virtualisation.containers = {
      containersConf.cniPlugins = [
        pkgs.cni-plugins
        pkgs.calico-cni-plugin
        pkgs.calico-kube-controllers
        pkgs.cni-plugin-flannel
      ];
    };

    systemd.services = {
      nvidia-container-toolkit-cdi-generator = {
        # Even with `--library-search-path`, `nvidia-ctk` won't find the libs
        # unless I bodge their path into the environment.
        environment.LD_LIBRARY_PATH = "${config.hardware.nvidia.package}/lib";
      };
    };

    systemd.services.mount-railbird-bucket = {
      after = ["agenix.service"];
      wantedBy = [ "multi-user.target" ];
      description = "Mount railbird bucket";
      serviceConfig = {
        Type = "simple";
        RemainAfterExit = true;
        Restart = "on-failure";  # Restart the service on failure
        RestartSec = 5;  # Wait 5 seconds before restarti
        TimeoutStopSec = 2;
        ExecStartPre = [
          "-${pkgs.util-linux}/bin/umount -f ${mount-path}"
          "${pkgs.coreutils}/bin/mkdir -p ${mount-path}"
          "${pkgs.coreutils}/bin/chown railbird:users ${mount-path}"
          "${pkgs.coreutils}/bin/chmod 0775 ${mount-path}"
        ];
        ExecStart = let
          key-file = config.age.secrets.api-service-key.path;
        in
        pkgs.writeShellScript "mount-railbird-bucket" ''
            while true; do
            if ${pkgs.util-linux}/bin/mount | grep -q "${mount-path}" && [ -d "${mount-path}/dev" ]; then
            echo "Mount path ${mount-path} is mounted and valid (contains directory 'dev')."
            else
            echo "Mount path is not valid or not mounted, attempting remount."
            ${pkgs.util-linux}/bin/umount -f "${mount-path}" || true
            ${pkgs.gcsfuse}/bin/gcsfuse --implicit-dirs --key-file "${key-file}" "${bucket-name}" "${mount-path}"
            fi
            echo "Sleeping"
            sleep 30
            done
          '';
        User = "root";
      };
    };

    services.k3s = {
      enable = true;
      clusterInit = cfg.serverAddr == "";
      serverAddr = cfg.serverAddr;
      configPath = pkgs.writeTextFile {
        name = "k3s-config.yaml";
        text = ''
          kubelet-arg:
          - "eviction-hard=nodefs.available<2Gi"
          - "eviction-soft=nodefs.available<5Gi"
          - "eviction-soft-grace-period=nodefs.available=5m"
        '';
      };
      tokenFile = config.age.secrets."1896Folsom-k3s-token.age".path;
      extraFlags =
        [
          "--tls-san ryzen-shine.local"
          "--tls-san nixquick.local"
          "--tls-san biskcomp.local"
          "--tls-san jimi-hendnix.local"
          "--tls-san dev.railbird.ai"
          "--node-label nixos-nvidia-cdi=enabled"
        ]
        ++ cfg.extraFlags;
      containerdConfigTemplate = ''
        {{ template "base" . }}

        plugins."io.containerd.grpc.v1.cri".cdi_spec_dirs = [ "/var/run/cdi" ]
        plugins."io.containerd.grpc.v1.cri".enable_cdi = true

        [debug]
        level = "trace"
      '';
      gracefulNodeShutdown = {
        enable = true;
      };
    };
  };
}
