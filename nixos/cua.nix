{ config, lib, ... }:

let
  cfg = config.myModules.cua;
  flavorDefaults = {
    xfce = {
      image = "trycua/cua-xfce:latest";
      storageMountPath = "/home/cua/storage";
      apiContainerPort = 8000;
      noVncContainerPort = 6901;
    };
    kasm = {
      image = "trycua/cua-ubuntu:latest";
      storageMountPath = "/home/kasm-user/storage";
      apiContainerPort = 8000;
      noVncContainerPort = 6901;
    };
    qemu-linux = {
      image = "trycua/cua-qemu-linux:latest";
      storageMountPath = "/storage";
      apiContainerPort = 5000;
      noVncContainerPort = 8006;
    };
  };
  selectedFlavor = flavorDefaults.${cfg.flavor};
  usingQemu = cfg.flavor == "qemu-linux";
in
{
  options.myModules.cua = {
    enable = lib.mkEnableOption "Cua Linux computer-use sandbox";

    android = {
      enable = lib.mkEnableOption "Cua Android QEMU computer-use sandbox";

      image = lib.mkOption {
        type = lib.types.str;
        default = "trycua/cua-qemu-android:latest";
        description = "OCI image to run for the Cua Android QEMU sandbox.";
      };

      bindAddress = lib.mkOption {
        type = lib.types.str;
        default = "127.0.0.1";
        description = "Address on which to expose the Cua Android API and VNC web ports.";
      };

      apiPort = lib.mkOption {
        type = lib.types.port;
        default = 8001;
        description = "Host port for the Cua Android computer-server API.";
      };

      webVncPort = lib.mkOption {
        type = lib.types.port;
        default = 6080;
        description = "Host port for the Cua Android VNC web UI.";
      };

      emulatorDevice = lib.mkOption {
        type = lib.types.str;
        default = "Samsung Galaxy S10";
        description = "Android emulator device profile.";
      };

      autoStart = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = "Whether to start the Cua Android sandbox automatically.";
      };

      openFirewall = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = "Whether to open the Cua Android API and VNC web ports in the firewall.";
      };

      extraOptions = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
        description = "Extra options passed to the Cua Android container runtime.";
      };
    };

    flavor = lib.mkOption {
      type = lib.types.enum [ "xfce" "kasm" "qemu-linux" ];
      default = "xfce";
      description = "Cua Linux sandbox flavor to run.";
    };

    image = lib.mkOption {
      type = lib.types.str;
      default = selectedFlavor.image;
      defaultText = "upstream default image for the selected Cua flavor";
      description = "OCI image to run for the Cua Linux sandbox.";
    };

    storageDir = lib.mkOption {
      type = lib.types.path;
      default = "/var/lib/cua/linux";
      description = "Host directory used for persistent Cua VM storage.";
    };

    bindAddress = lib.mkOption {
      type = lib.types.str;
      default = "127.0.0.1";
      description = "Address on which to expose the Cua API and noVNC ports.";
    };

    apiPort = lib.mkOption {
      type = lib.types.port;
      default = selectedFlavor.apiContainerPort;
      defaultText = "upstream API port for the selected Cua flavor";
      description = "Host port for the Cua computer-server API.";
    };

    noVncPort = lib.mkOption {
      type = lib.types.port;
      default = selectedFlavor.noVncContainerPort;
      defaultText = "upstream noVNC port for the selected Cua flavor";
      description = "Host port for the Cua noVNC web UI.";
    };

    vncPort = lib.mkOption {
      type = lib.types.nullOr lib.types.port;
      default = 5901;
      description = "Optional host port for direct VNC access. Only used by the XFCE flavor.";
    };

    vncResolution = lib.mkOption {
      type = lib.types.str;
      default = "1024x768";
      description = "VNC desktop resolution for the Cua XFCE container.";
    };

    ramSize = lib.mkOption {
      type = lib.types.str;
      default = "8G";
      description = "RAM allocated to the Cua QEMU VM.";
    };

    cpuCores = lib.mkOption {
      type = lib.types.ints.positive;
      default = 4;
      description = "CPU cores allocated to the Cua QEMU VM.";
    };

    diskSize = lib.mkOption {
      type = lib.types.str;
      default = "64G";
      description = "Disk size allocated to the Cua QEMU VM. Only used by the QEMU Linux flavor.";
    };

    shmSize = lib.mkOption {
      type = lib.types.str;
      default = "512m";
      description = "Shared-memory size for non-QEMU Cua desktop containers.";
    };

    openFirewall = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Whether to open the Cua API and noVNC ports in the firewall.";
    };

    autoStart = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Whether to start the Cua sandbox container automatically.";
    };

    extraOptions = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      description = "Extra options passed to the container runtime.";
    };
  };

  config = lib.mkIf cfg.enable {
    virtualisation.docker.enable = true;

    systemd.tmpfiles.rules = [
      "d ${toString cfg.storageDir} 0750 root root -"
    ];

    virtualisation.oci-containers.containers = {
      cua-sandbox = {
        image = cfg.image;
        autoStart = cfg.autoStart;
        ports = [
          "${cfg.bindAddress}:${toString cfg.noVncPort}:${toString selectedFlavor.noVncContainerPort}"
          "${cfg.bindAddress}:${toString cfg.apiPort}:${toString selectedFlavor.apiContainerPort}"
        ]
        ++ lib.optionals (cfg.flavor == "xfce" && cfg.vncPort != null) [
          "${cfg.bindAddress}:${toString cfg.vncPort}:5901"
        ];
        volumes = [
          "${toString cfg.storageDir}:${selectedFlavor.storageMountPath}"
        ];
        devices = lib.optionals usingQemu [
          "/dev/kvm:/dev/kvm"
        ];
        capabilities = lib.optionalAttrs usingQemu {
          NET_ADMIN = true;
        };
        environment =
          lib.optionalAttrs usingQemu {
            RAM_SIZE = cfg.ramSize;
            CPU_CORES = toString cfg.cpuCores;
            DISK_SIZE = cfg.diskSize;
          }
          // lib.optionalAttrs (cfg.flavor == "xfce") {
            VNC_RESOLUTION = cfg.vncResolution;
          }
          // lib.optionalAttrs (cfg.flavor == "kasm") {
            VNCOPTIONS = "-disableBasicAuth";
          };
        extraOptions =
          lib.optionals (!usingQemu) [ "--shm-size=${cfg.shmSize}" ]
          ++ cfg.extraOptions;
      };
      cua-android = lib.mkIf cfg.android.enable {
        image = cfg.android.image;
        autoStart = cfg.android.autoStart;
        ports = [
          "${cfg.android.bindAddress}:${toString cfg.android.webVncPort}:6080"
          "${cfg.android.bindAddress}:${toString cfg.android.apiPort}:8000"
        ];
        devices = [
          "/dev/kvm:/dev/kvm"
        ];
        environment = {
          EMULATOR_DEVICE = cfg.android.emulatorDevice;
          WEB_VNC = "true";
        };
        extraOptions = cfg.android.extraOptions;
      };
    };

    networking.firewall.allowedTCPPorts =
      lib.optionals cfg.openFirewall (
        [ cfg.apiPort cfg.noVncPort ]
        ++ lib.optional (cfg.flavor == "xfce" && cfg.vncPort != null) cfg.vncPort
      )
      ++ lib.optionals cfg.android.openFirewall [ cfg.android.apiPort cfg.android.webVncPort ];
  };
}
