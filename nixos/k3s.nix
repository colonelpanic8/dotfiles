{ config, lib, ... }:
with lib;
let cfg = config.myModules.railbird-k3s;
in {
  options = {
    myModules.railbird-k3s = {
      enable = mkEnableOption "railbird k3s";
      serverAddr = mkOption {
        type = lib.types.str;
        default = "";
      };
    };
  };
  config = mkIf cfg.enable {
    age.secrets."1896Folsom-k3s-token.age".file = ./secrets/1896Folsom-k3s-token.age;
    age.secrets."k3s-registry.yaml.age".file = ./secrets/k3s-registry.yaml.age;
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
        plugins."io.containerd.grpc.v1.cri" = {
          enable_cdi = true;
          cdi_spec_dirs = [ "/var/run/cdi" ];
        };
      };
    };
    services.k3s = {
      enable = true;
      clusterInit = cfg.serverAddr == "";
      serverAddr = cfg.serverAddr;
      tokenFile = config.age.secrets."1896Folsom-k3s-token.age".path;
      extraFlags = [
        "--tls-san ryzen-shine.local"
        "--tls-san nixquick.local"
        "--tls-san biskcomp.local"
        "--tls-san jimi-hendnix.local"
        "--tls-san dev.railbird.ai"
        "--container-runtime-endpoint unix:///run/containerd/containerd.sock"
      ];
      containerdConfigTemplate = ''
        {{ template "base" . }}

        [plugins."io.containerd.grpc.v1.cri".containerd.runtimes.nvidia]
        privileged_without_host_devices = false
        runtime_engine = ""
        runtime_root = ""
        runtime_type = "io.containerd.runc.v2"

        [plugins."io.containerd.grpc.v1.cri".containerd.runtimes.nvidia.options]
        BinaryName = "/run/current-system/sw/bin/nvidia-container-runtime"
      '';
      gracefulNodeShutdown = {
        enable = true;
      };
    };
  };
}
