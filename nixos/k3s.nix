{ config, makeEnable, ... }:
makeEnable config "myModules.railbird-k3s" false {
  services.k3s = {
    enable = true;
    role = "server";
    clusterInit = true;
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
}
