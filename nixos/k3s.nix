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
    services.dockerRegistry = {
      enable = true;
      listenAddress = "0.0.0.0";
      port = 5279;
      enableDelete = true;
      enableGarbageCollect = true;
    };
    services.k3s = {
      enable = true;
      role = "server";
      clusterInit = cfg.serverAddr == "";
      serverAddr = cfg.serverAddr;
      tokenFile = config.age.secrets."1896Folsom-k3s-token.age".path;
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
