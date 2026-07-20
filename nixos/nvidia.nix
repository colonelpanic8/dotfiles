{
  config,
  pkgs,
  makeEnable,
  lib,
  ...
}: let
  # Querying a runtime-suspended NVIDIA GPU with nvidia-smi wakes it. On hybrid
  # laptops that is both wasteful and, with affected drivers, can trigger GSP
  # runtime-resume failures. Status widgets should use this wrapper so a
  # sleeping dGPU remains asleep; explicit user/compute invocations can keep
  # using nvidia-smi directly.
  nvidiaSmiNoWake = pkgs.writeShellApplication {
    name = "nvidia-smi-no-wake";
    runtimeInputs = [config.hardware.nvidia.package.bin];
    text = ''
      found_device=false
      for status_file in /sys/bus/pci/drivers/nvidia/[0-9]*:[0-9]*:[0-9]*.[0-9]/power/runtime_status; do
        if [[ ! -r "$status_file" ]]; then
          continue
        fi
        found_device=true
        case "$(<"$status_file")" in
          suspended | suspending) ;;
          *) exec nvidia-smi "$@" ;;
        esac
      done

      if [[ "$found_device" == false ]]; then
        exec nvidia-smi "$@"
      fi

      exit 1
    '';
  };
  nvidiaPowerdVersionCondition = pkgs.writeShellScript "nvidia-powerd-version-matches" ''
    set -euo pipefail
    version_file=/proc/driver/nvidia/version
    if [[ ! -r "$version_file" ]]; then
      exit 0
    fi

    version_line="$(<"$version_file")"
    if [[ "$version_line" =~ ([0-9]+\.[0-9]+(\.[0-9]+)?) ]]; then
      loaded_version="''${BASH_REMATCH[1]}"
      configured_version=${lib.escapeShellArg config.hardware.nvidia.package.version}
      if [[ "$loaded_version" != "$configured_version" ]]; then
        echo "Skipping nvidia-powerd until reboot: loaded module $loaded_version != userspace $configured_version" >&2
        exit 1
      fi
    fi
  '';
in
  makeEnable config "myModules.nvidia" false {
    environment.systemPackages = with pkgs; [
      nvidia-container-toolkit
      nvidia-container-toolkit.tools
      nvidiaSmiNoWake
    ];
    hardware.nvidia-container-toolkit = {
      enable = true;
      mount-nvidia-executables = true;
      # additionalEdit = ''
      #   ${lib.getExe pkgs.jq} '
      #       .devices |= map(
      #       .containerEdits.hooks |= map(select(.args | index("nvidia-cdi-hook") < 0))
      #       )' | ${lib.getExe pkgs.jq} '.containerEdits.hooks |= map(select(.args | index("nvidia-cdi-hook") < 0 ))' '';
    };
    hardware.nvidia.open = true;
    hardware.graphics.enable32Bit = true;
    hardware.graphics.extraPackages = [config.hardware.nvidia.package.out];
    hardware.graphics.extraPackages32 = [config.hardware.nvidia.package.lib32];
    systemd.services.nvidia-container-toolkit-cdi-generator.serviceConfig = {
      # During `nixos-rebuild switch`, the NVIDIA userspace package can be newer
      # than the already-loaded kernel module. In that transient state NVML exits
      # with a driver/library mismatch, but the generator succeeds again after the
      # reboot that loads the matching module.
      SuccessExitStatus = ["1"];
    };
    # A switch can install newer NVIDIA userspace while the old kernel module
    # remains loaded until reboot. Avoid restarting the driver-bound daemon in
    # that mismatched window; it starts normally with the matching module at boot.
    systemd.services.nvidia-powerd = {
      restartIfChanged = false;
      serviceConfig.ExecCondition = nvidiaPowerdVersionCondition;
    };
    services.xserver = {
      videoDrivers = ["nvidia"];
    };
    # NVIDIA machines are the capable desktops, so run the local Kokoro TTS
    # server (kokoro_speak / "read selection aloud") on them by default.
    myModules.kokoro.enable = lib.mkDefault true;
    # nixpkgs.config.cudaSupport = true;
  }
