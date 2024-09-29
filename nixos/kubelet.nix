{ config, pkgs, makeEnable, ... }:
makeEnable config "myModules.kubelet" false {
  age.secrets."api_service_account_key.json.age".file = ./secrets/api_service_account_key.json.age;
  services.kubernetes.kubelet = {
    enable = true;
    kubeconfig = {
      server = "https://34.31.205.230";
      caFile = ./railbird-kubernetes.crt;
      keyFile = config.age.secrets."api_service_account_key.json.age".path;
    };
    registerNode = true;
    cni = {
      packages = [ pkgs.cni-plugins ];
    };
    extraOpts = ''
      --fail-swap-on=false
      --container-runtime=remote
      --container-runtime-endpoint=unix:///run/containerd/containerd.sock
    '';
  };
}
