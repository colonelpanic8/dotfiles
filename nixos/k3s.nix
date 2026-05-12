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
          image: nvcr.io/nvidia/cuda:12.6.3-base-ubuntu24.04
          command: ["nvidia-smi"]
          resources:
            limits:
              nvidia.com/gpu: 1
  '';
in {
  options = {
    myModules.railbird-k3s = {
      enable = mkEnableOption "railbird k3s";
      serverAddr = mkOption {
        type = lib.types.str;
        default = "";
        description = ''
          Existing k3s server URL to join. Leave empty for the first or only
          server; that enables cluster initialization with no peers.
        '';
      };
      extraFlags = mkOption {
        type = lib.types.listOf lib.types.str;
        default = [];
      };
    };
  };
  config = mkIf cfg.enable {
    users.users.railbird = {
      isSystemUser = true;
      group = "users";
      home = "/var/lib/railbird";
      createHome = true;
    };

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
          cdi_spec_dirs = ["/var/run/cdi"];
        };
        plugins."io.containerd.grpc.v1.cri" = {
          enable_cdi = true;
          cdi_spec_dirs = ["/var/run/cdi"];
          cni.bin_dir = "${plugins-path}/bin";
        };
      };
    };

    hardware.nvidia-container-toolkit = {
      enable = true;
      device-name-strategy = "uuid";
      mount-nvidia-executables = true;
    };
    virtualisation.containers = {
      containersConf.cniPlugins = [
        pkgs.cni-plugins
        pkgs.calico-cni-plugin
        pkgs.calico-kube-controllers
        pkgs.cni-plugin-flannel
      ];
    };

    systemd.services = {
      # k3s can sit in sd_notify startup indefinitely while waiting for remote
      # etcd peers. Treat it as a long-running service so nixos-rebuild switch
      # does not block on cluster readiness.
      k3s.serviceConfig = {
        Type = mkForce "simple";
      };

      nvidia-container-toolkit-cdi-generator = {
        # Even with `--library-search-path`, `nvidia-ctk` won't find the libs
        # unless I bodge their path into the environment.
        environment.LD_LIBRARY_PATH = "${config.hardware.nvidia.package}/lib";
      };
    };

    systemd.services.mount-railbird-bucket = {
      after = ["agenix.service"];
      wantedBy = ["multi-user.target"];
      description = "Mount railbird bucket";
      serviceConfig = {
        Type = "simple";
        RemainAfterExit = true;
        Restart = "on-failure"; # Restart the service on failure
        RestartSec = 5; # Wait 5 seconds before restarti
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
          "--disable=traefik"
          "--disable=servicelb"
          "--node-label nixos-nvidia-cdi=enabled"
          "--node-label nvidia.com/gpu.present=true"
          "--etcd-arg=quota-backend-bytes=8589934592"
        ]
        ++ cfg.extraFlags;
      containerdConfigTemplate = ''
        {{ template "base" . }}

        plugins."io.containerd.grpc.v1.cri".cdi_spec_dirs = [ "/var/run/cdi" ]
        plugins."io.containerd.grpc.v1.cri".enable_cdi = true

        [plugins."io.containerd.grpc.v1.cri".containerd.runtimes.nvidia]
        privileged_without_host_devices = false
        runtime_engine = ""
        runtime_root = ""
        runtime_type = "io.containerd.runc.v2"

        [plugins."io.containerd.grpc.v1.cri".containerd.runtimes.nvidia.options]
        BinaryName = "${lib.getOutput "tools" config.hardware.nvidia-container-toolkit.package}/bin/nvidia-container-runtime.cdi"

        [debug]
        level = "trace"
      '';
      gracefulNodeShutdown = {
        enable = true;
      };
    };

    environment.systemPackages = with pkgs; [
      kubectl
      kubernetes-helm
      nvidia-container-toolkit
      nvidia-container-toolkit.tools
    ];

    environment.etc."k3s/gpu-test-pod.yaml".source = gpu-test-pod;
    environment.etc."k3s/nvidia-device-plugin.yaml".source = nvidia-device-plugin-manifest;

    systemd.services.k3s-gpu-plugin-deploy = {
      description = "Deploy NVIDIA device plugin to k3s";
      after = ["k3s.service"];
      wants = ["k3s.service"];
      wantedBy = ["multi-user.target"];
      path = [pkgs.kubectl pkgs.coreutils];
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        ExecStart = pkgs.writeShellScript "deploy-nvidia-device-plugin" ''
          export KUBECONFIG=/etc/rancher/k3s/k3s.yaml

          echo "Waiting for k3s API server..."
          for i in $(seq 1 60); do
            if kubectl get nodes &>/dev/null; then
              echo "k3s API server is ready"
              break
            fi
            sleep 5
          done

          kubectl delete daemonset -n kube-system generic-cdi-plugin --ignore-not-found=true
          kubectl apply -f ${nvidia-device-plugin-manifest}
          kubectl rollout status daemonset/nvidia-device-plugin-daemonset -n kube-system --timeout=120s || true
        '';
      };
    };
  };
}
