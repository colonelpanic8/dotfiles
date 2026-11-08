# Google Drive mounted at ~/google-drive through rclone. The "gdrive" remote,
# OAuth token included, comes from an agenix secret; without that file the
# module adds nothing, and a host that cannot decrypt it skips the mount.
# Offline, cached files and listings stay readable and writes queue in the VFS
# cache; a dead mount is lazily unmounted so it cannot wedge shells.
{
  config,
  lib,
  pkgs,
  ...
}: let
  remote = "gdrive";
  secretFile = ../../secrets/google-drive-rclone.conf.age;
in {
  config = lib.mkIf (config.myUsers.imalison.has.full && builtins.pathExists secretFile) {
    home-manager.users.imalison = {config, ...}: let
      mountPoint = "${config.home.homeDirectory}/google-drive";
      secretPath = config.age.secrets.google-drive-rclone.path;
      fusermount = "/run/wrappers/bin/fusermount3";
      # rclone writes refreshed access tokens back, so it needs a writable copy.
      rcloneConfig = ''"$XDG_RUNTIME_DIR/google-drive/rclone.conf"'';
      haveSecret = pkgs.writeShellScript "google-drive-have-secret" ''
        test -s "${secretPath}"
      '';
      start = pkgs.writeShellScript "google-drive-mount" ''
        set -eu
        ${fusermount} -uz ${mountPoint} 2>/dev/null || true
        mkdir -p ${mountPoint} "$XDG_RUNTIME_DIR/google-drive"
        install -m 600 "${secretPath}" ${rcloneConfig}
        exec ${lib.getExe pkgs.rclone} mount ${remote}: ${mountPoint} \
          --config ${rcloneConfig} \
          --vfs-cache-mode=full \
          --vfs-cache-max-size=20G \
          --vfs-cache-max-age=720h \
          --dir-cache-time=1000h \
          --poll-interval=1m \
          --attr-timeout=10s \
          --contimeout=15s \
          --timeout=30s \
          --low-level-retries=3 \
          --drive-export-formats=link.html
      '';
    in {
      age.secrets.google-drive-rclone.file = secretFile;

      systemd.user.services.google-drive = {
        Unit = {
          Description = "Google Drive rclone mount";
          After = ["agenix.service"];
          Wants = ["agenix.service"];
          StartLimitIntervalSec = 0;
        };
        Install.WantedBy = ["default.target"];
        Service = {
          Type = "notify";
          Environment = ["PATH=/run/wrappers/bin:${lib.makeBinPath [pkgs.coreutils]}"];
          ExecCondition = "${haveSecret}";
          ExecStart = "${start}";
          ExecStopPost = "-${fusermount} -uz ${mountPoint}";
          SuccessExitStatus = "143";
          Restart = "on-failure";
          RestartSec = "1min";
          TimeoutStopSec = "20s";
        };
      };
    };
  };
}
