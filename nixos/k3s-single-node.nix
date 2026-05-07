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

  nvidia-device-plugin-version = "v0.19.1";

  nvidia-device-plugin-manifest = pkgs.writeText "nvidia-device-plugin.yaml" ''
    apiVersion: node.k8s.io/v1
    handler: nvidia
    kind: RuntimeClass
    metadata:
      name: nvidia
      labels:
        app.kubernetes.io/component: gpu-operator
    ---
    apiVersion: apps/v1
    kind: DaemonSet
    metadata:
      name: nvidia-device-plugin-daemonset
      namespace: kube-system
      labels:
        app.kubernetes.io/name: nvidia-device-plugin
    spec:
      selector:
        matchLabels:
          app.kubernetes.io/name: nvidia-device-plugin
      updateStrategy:
        type: RollingUpdate
      template:
        metadata:
          labels:
            app.kubernetes.io/name: nvidia-device-plugin
        spec:
          runtimeClassName: nvidia
          priorityClassName: system-node-critical
          nodeSelector:
            nvidia.com/gpu.present: "true"
          tolerations:
            - key: nvidia.com/gpu
              operator: Exists
              effect: NoSchedule
          containers:
            - name: nvidia-device-plugin-ctr
              image: nvcr.io/nvidia/k8s-device-plugin:${nvidia-device-plugin-version}
              imagePullPolicy: IfNotPresent
              command: ["nvidia-device-plugin"]
              env:
                - name: DEVICE_ID_STRATEGY
                  value: uuid
                - name: NVIDIA_VISIBLE_DEVICES
                  value: all
                - name: NVIDIA_DRIVER_CAPABILITIES
                  value: compute,utility
              securityContext:
                allowPrivilegeEscalation: false
                capabilities:
                  drop: ["ALL"]
              volumeMounts:
                - name: kubelet-device-plugins-dir
                  mountPath: /var/lib/kubelet/device-plugins
                - name: cdi-specs
                  mountPath: /var/run/cdi
                  readOnly: true
          volumes:
            - name: kubelet-device-plugins-dir
              hostPath:
                path: /var/lib/kubelet/device-plugins
                type: Directory
            - name: cdi-specs
              hostPath:
                path: /var/run/cdi
                type: DirectoryOrCreate
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
      runtimeClassName: nvidia
      containers:
        - name: cuda-test
          image: nvidia/cuda:12.6.3-base-ubuntu24.04
          command: ["nvidia-smi"]
          resources:
            limits:
              nvidia.com/gpu: 1
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
    hardware.nvidia-container-toolkit = {
      enable = true;
      device-name-strategy = "uuid";
      mount-nvidia-executables = true;
    };

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

      extraFlags =
        [
          "--node-label=nixos-nvidia-cdi=enabled"
          "--node-label=nvidia.com/gpu.present=true"
          "--tls-san=${config.networking.hostName}"
          "--tls-san=${config.networking.hostName}.local"
          "--tls-san=localhost"
        ]
        ++ cfg.extraFlags;

      # Containerd config with CDI support
      # k3s 1.31+ with containerd 2.0 has CDI enabled by default
      # We only need to add the nvidia runtime configuration
      containerdConfigTemplate = ''
        {{ template "base" . }}

        [plugins."io.containerd.grpc.v1.cri".containerd.runtimes.nvidia]
        privileged_without_host_devices = false
        runtime_engine = ""
        runtime_root = ""
        runtime_type = "io.containerd.runc.v2"

        [plugins."io.containerd.grpc.v1.cri".containerd.runtimes.nvidia.options]
        BinaryName = "${lib.getOutput "tools" config.hardware.nvidia-container-toolkit.package}/bin/nvidia-container-runtime.cdi"
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

    # Create systemd service to deploy the NVIDIA device plugin after k3s is ready
    systemd.services.k3s-gpu-plugin-deploy = {
      description = "Deploy NVIDIA device plugin to k3s";
      after = ["k3s.service"];
      wants = ["k3s.service"];
      wantedBy = ["multi-user.target"];
      path = [pkgs.kubectl pkgs.coreutils];
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

          if kubectl get daemonset -n kube-system generic-cdi-plugin &>/dev/null; then
            echo "Removing old generic-cdi-plugin deployment..."
            kubectl delete daemonset -n kube-system generic-cdi-plugin --ignore-not-found=true
          fi

          echo "Deploying NVIDIA device plugin..."
          kubectl apply -f ${nvidia-device-plugin-manifest}

          echo "Waiting for NVIDIA device plugin to be ready..."
          kubectl rollout status daemonset/nvidia-device-plugin-daemonset -n kube-system --timeout=120s || true
        '';
      };
    };

    # Store test manifests in /etc for easy access
    environment.etc."k3s/gpu-test-pod.yaml".source = gpu-test-pod;
    environment.etc."k3s/nvidia-device-plugin.yaml".source = nvidia-device-plugin-manifest;
  };
}
