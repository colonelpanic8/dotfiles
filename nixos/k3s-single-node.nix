{
  pkgs,
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.myModules.k3s-single-node;
  plugins-path = pkgs.buildEnv {
    name = "combined-cni-plugins";
    paths = [
      pkgs.cni-plugins
      pkgs.cni-plugin-flannel
    ];
  };

  # Generic CDI Plugin DaemonSet for GPU resource allocation
  generic-cdi-plugin-manifest = pkgs.writeText "generic-cdi-plugin.yaml" ''
    apiVersion: apps/v1
    kind: DaemonSet
    metadata:
      name: generic-cdi-plugin
      namespace: kube-system
      labels:
        app: generic-cdi-plugin
    spec:
      selector:
        matchLabels:
          app: generic-cdi-plugin
      template:
        metadata:
          labels:
            app: generic-cdi-plugin
        spec:
          nodeSelector:
            nixos-nvidia-cdi: "enabled"
          tolerations:
            - key: nvidia.com/gpu
              operator: Exists
              effect: NoSchedule
          containers:
            - name: generic-cdi-plugin
              image: ghcr.io/olfillasodikno/generic-cdi-plugin:main
              imagePullPolicy: Always
              args:
                - "/var/run/cdi/nvidia-container-toolkit.json"
              securityContext:
                privileged: true
              volumeMounts:
                - name: device-plugin
                  mountPath: /var/lib/kubelet/device-plugins
                - name: pod-resources
                  mountPath: /var/lib/kubelet/pod-resources
                - name: cdi-specs
                  mountPath: /var/run/cdi
                  readOnly: true
          volumes:
            - name: device-plugin
              hostPath:
                path: /var/lib/kubelet/device-plugins
            - name: pod-resources
              hostPath:
                path: /var/lib/kubelet/pod-resources
            - name: cdi-specs
              hostPath:
                path: /var/run/cdi
  '';

  # Test pod to verify GPU access
  gpu-test-pod = pkgs.writeText "gpu-test-pod.yaml" ''
    apiVersion: v1
    kind: Pod
    metadata:
      name: gpu-test
      namespace: default
    spec:
      restartPolicy: Never
      containers:
        - name: cuda-test
          image: nvidia/cuda:12.6.3-base-ubuntu24.04
          command: ["nvidia-smi"]
          resources:
            limits:
              nvidia.com/gpu-all: 1
  '';
in {
  options = {
    myModules.k3s-single-node = {
      enable = mkEnableOption "single-node k3s with GPU/CDI support";
      extraFlags = mkOption {
        type = lib.types.listOf lib.types.str;
        default = [];
        description = "Extra flags to pass to k3s";
      };
    };
  };

  config = mkIf cfg.enable {
    # NVIDIA container toolkit for CDI spec generation
    hardware.nvidia-container-toolkit.enable = true;

    # Ensure CDI generator has access to nvidia libs
    systemd.services.nvidia-container-toolkit-cdi-generator = {
      environment.LD_LIBRARY_PATH = "${config.hardware.nvidia.package}/lib";
    };

    # k3s configuration
    services.k3s = {
      enable = true;
      role = "server";
      clusterInit = true;

      configPath = pkgs.writeTextFile {
        name = "k3s-config.yaml";
        text = ''
          # Disable servicelb and traefik for a minimal single-node setup
          disable:
            - servicelb
            - traefik
          kubelet-arg:
            - "eviction-hard=nodefs.available<2Gi"
            - "eviction-soft=nodefs.available<5Gi"
            - "eviction-soft-grace-period=nodefs.available=5m"
        '';
      };

      extraFlags = [
        "--node-label=nixos-nvidia-cdi=enabled"
        "--tls-san=${config.networking.hostName}"
        "--tls-san=${config.networking.hostName}.local"
        "--tls-san=localhost"
      ] ++ cfg.extraFlags;

      # Containerd config with CDI support
      # k3s 1.31+ with containerd 2.0 has CDI enabled by default
      # We only need to add the nvidia runtime configuration
      containerdConfigTemplate = ''
        {{ template "base" . }}

        [plugins."io.containerd.grpc.v1.cri".containerd.runtimes.nvidia]
        runtime_type = "io.containerd.runc.v2"

        [plugins."io.containerd.grpc.v1.cri".containerd.runtimes.nvidia.options]
        BinaryName = "/run/current-system/sw/bin/nvidia-container-runtime.cdi"
      '';

      gracefulNodeShutdown.enable = true;
    };

    # Make nvidia-container-toolkit available in system PATH
    environment.systemPackages = with pkgs; [
      nvidia-container-toolkit
      nvidia-container-toolkit.tools
      kubectl
      kubernetes-helm
    ];

    # Symlink nvidia-container-runtime.cdi to system path
    environment.etc."profile.d/k3s-gpu.sh".text = ''
      export KUBECONFIG=/etc/rancher/k3s/k3s.yaml
    '';

    # Create systemd service to deploy the generic-cdi-plugin after k3s is ready
    systemd.services.k3s-gpu-plugin-deploy = {
      description = "Deploy generic-cdi-plugin to k3s";
      after = [ "k3s.service" ];
      wants = [ "k3s.service" ];
      wantedBy = [ "multi-user.target" ];
      path = [ pkgs.kubectl pkgs.coreutils ];
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        ExecStart = pkgs.writeShellScript "deploy-cdi-plugin" ''
          export KUBECONFIG=/etc/rancher/k3s/k3s.yaml

          # Wait for k3s API to be ready
          echo "Waiting for k3s API server..."
          for i in $(seq 1 60); do
            if kubectl get nodes &>/dev/null; then
              echo "k3s API server is ready"
              break
            fi
            sleep 5
          done

          # Check if plugin already exists
          if kubectl get daemonset -n kube-system generic-cdi-plugin &>/dev/null; then
            echo "generic-cdi-plugin already deployed, updating..."
            kubectl apply -f ${generic-cdi-plugin-manifest}
          else
            echo "Deploying generic-cdi-plugin..."
            kubectl apply -f ${generic-cdi-plugin-manifest}
          fi

          echo "Waiting for generic-cdi-plugin to be ready..."
          kubectl rollout status daemonset/generic-cdi-plugin -n kube-system --timeout=120s || true
        '';
      };
    };

    # Store test manifests in /etc for easy access
    environment.etc."k3s/gpu-test-pod.yaml".source = gpu-test-pod;
    environment.etc."k3s/generic-cdi-plugin.yaml".source = generic-cdi-plugin-manifest;
  };
}
