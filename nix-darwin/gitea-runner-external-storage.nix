{
  config,
  lib,
  pkgs,
  ...
}: let
  runnerUser = config.services.gitea-actions-runner.user;
  runnerInstance = config.services.gitea-actions-runner.instances.nix;
  runnerPackage = config.services.gitea-actions-runner.package;
  settingsFormat = pkgs.formats.yaml {};

  externalVolume = "/Volumes/Extreme SSD";
  imageDirectory = "${externalVolume}/railbird-runner";
  imagePath = "${imageDirectory}/runner.sparsebundle";
  runnerMountPoint = "/private/var/lib/gitea-runner-external";
  runnerWorkDirectory = "${runnerMountPoint}/work";
  runnerCacheDirectory = "${runnerMountPoint}/cache";

  # The image grows on demand but cannot consume the entire external disk.
  imageCapacityGiB = 64;
  # Do not provision it until cleanup has left useful headroom on the SSD.
  minimumProvisioningFreeKiB = 80 * 1024 * 1024;

  mountRunnerStorage = pkgs.writeShellScript "mount-gitea-runner-storage" ''
    set -u

    is_mounted() {
      /sbin/mount | /usr/bin/grep -Fq ' on ${runnerMountPoint} ('
    }

    while true; do
      if is_mounted; then
        /bin/sleep 30
        continue
      fi

      if [ ! -d ${lib.escapeShellArg externalVolume} ]; then
        echo "gitea runner storage: waiting for ${externalVolume}" >&2
        /bin/sleep 60
        continue
      fi

      /bin/mkdir -p ${lib.escapeShellArg imageDirectory} ${lib.escapeShellArg runnerMountPoint}

      if [ ! -e ${lib.escapeShellArg imagePath} ]; then
        available_kib="$(/bin/df -Pk ${lib.escapeShellArg externalVolume} | /usr/bin/awk 'NR == 2 { print $4 }')"
        if [ -z "$available_kib" ] || [ "$available_kib" -lt ${toString minimumProvisioningFreeKiB} ]; then
          echo "gitea runner storage: at least 80 GiB must be free before provisioning ${imagePath}" >&2
          /bin/sleep 60
          continue
        fi

        echo "gitea runner storage: creating capped ${toString imageCapacityGiB} GiB APFS sparse bundle"
        if ! /usr/bin/hdiutil create \
          -size ${toString imageCapacityGiB}g \
          -fs APFS \
          -type SPARSEBUNDLE \
          -volname RailbirdRunner \
          ${lib.escapeShellArg imagePath}; then
          /bin/sleep 60
          continue
        fi
      fi

      if ! /usr/bin/hdiutil attach \
        -nobrowse \
        -owners on \
        -mountpoint ${lib.escapeShellArg runnerMountPoint} \
        ${lib.escapeShellArg imagePath}; then
        echo "gitea runner storage: attach failed; runner will remain offline" >&2
        /bin/sleep 60
        continue
      fi

      /usr/sbin/chown ${lib.escapeShellArg "${runnerUser}:staff"} ${lib.escapeShellArg runnerMountPoint}
      /usr/bin/install -d -m 0755 -o ${lib.escapeShellArg runnerUser} -g staff \
        ${lib.escapeShellArg runnerWorkDirectory} \
        ${lib.escapeShellArg runnerCacheDirectory} \
        ${lib.escapeShellArg "${runnerCacheDirectory}/xdg"} \
        ${lib.escapeShellArg "${runnerCacheDirectory}/cargo"} \
        ${lib.escapeShellArg "${runnerCacheDirectory}/rustup"} \
        ${lib.escapeShellArg "${runnerCacheDirectory}/gradle"} \
        ${lib.escapeShellArg "${runnerCacheDirectory}/cocoapods"} \
        ${lib.escapeShellArg "${runnerCacheDirectory}/yarn"} \
        ${lib.escapeShellArg "${runnerCacheDirectory}/npm"} \
        ${lib.escapeShellArg "${runnerCacheDirectory}/go-build"} \
        ${lib.escapeShellArg "${runnerCacheDirectory}/go-mod"} \
        ${lib.escapeShellArg "${runnerCacheDirectory}/pip"} \
        ${lib.escapeShellArg "${runnerCacheDirectory}/uv"}
      /usr/bin/install -d -m 0700 -o ${lib.escapeShellArg runnerUser} -g staff \
        ${lib.escapeShellArg "${runnerCacheDirectory}/tmp"} \
        ${lib.escapeShellArg "${runnerCacheDirectory}/runtime"}
    done
  '';

  runnerConfig = settingsFormat.generate "gitea-runner-config.yaml" runnerInstance.settings;
  runWithExternalStorage = pkgs.writeShellScript "run-gitea-runner-with-external-storage" ''
    set -u

    for _ in $(${pkgs.coreutils}/bin/seq 1 24); do
      if /sbin/mount | /usr/bin/grep -Fq ' on ${runnerMountPoint} (' \
        && [ -w ${lib.escapeShellArg runnerWorkDirectory} ]; then
        cd /var/lib/gitea-runner/nix
        exec ${runnerPackage}/bin/gitea-runner daemon --config ${runnerConfig}
      fi
      /bin/sleep 5
    done

    echo "gitea runner: external APFS storage is unavailable; refusing internal-disk fallback" >&2
    exit 75
  '';
in {
  services.gitea-actions-runner.instances.nix.settings.host.workdir_parent =
    lib.mkForce runnerWorkDirectory;

  launchd.daemons.gitea-runner-storage = {
    serviceConfig = {
      ProgramArguments = ["${mountRunnerStorage}"];
      KeepAlive = true;
      ThrottleInterval = 30;
      ProcessType = "Background";
      StandardOutPath = "/var/log/gitea-runner-storage.log";
      StandardErrorPath = "/var/log/gitea-runner-storage.err.log";
    };
  };

  launchd.daemons.gitea-runner-nix.serviceConfig = {
    ProgramArguments = lib.mkForce ["${runWithExternalStorage}"];
    EnvironmentVariables = {
      XDG_CACHE_HOME = lib.mkForce "${runnerCacheDirectory}/xdg";
      XDG_RUNTIME_DIR = lib.mkForce "${runnerCacheDirectory}/runtime";
      TMPDIR = "${runnerCacheDirectory}/tmp";
      CARGO_HOME = "${runnerCacheDirectory}/cargo";
      RUSTUP_HOME = "${runnerCacheDirectory}/rustup";
      GRADLE_USER_HOME = "${runnerCacheDirectory}/gradle";
      CP_HOME_DIR = "${runnerCacheDirectory}/cocoapods";
      YARN_CACHE_FOLDER = "${runnerCacheDirectory}/yarn";
      npm_config_cache = "${runnerCacheDirectory}/npm";
      GOCACHE = "${runnerCacheDirectory}/go-build";
      GOMODCACHE = "${runnerCacheDirectory}/go-mod";
      PIP_CACHE_DIR = "${runnerCacheDirectory}/pip";
      UV_CACHE_DIR = "${runnerCacheDirectory}/uv";
    };
  };
}
