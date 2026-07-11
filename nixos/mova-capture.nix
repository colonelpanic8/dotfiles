# Quick-capture into mova / org-agenda-api.
#
# The rofi front-end (dotfiles/lib/bin/mova_capture.sh) drops a job file into
# ~/.local/state/mova-capture/queue and this module runs the drain worker that
# POSTs it to the API, retrying persistently until it lands:
#   - .path  fires the drain the moment a job is enqueued
#   - .timer retries anything still queued (network/API outages, reboots)
{
  config,
  pkgs,
  lib,
  ...
}: let
  worktree = config.dotfiles-worktree;
  drain = "${worktree}/dotfiles/lib/bin/mova_capture_drain";
  drainPath = lib.makeBinPath [
    pkgs.bash
    pkgs.coreutils
    pkgs.curl
    pkgs.jq
    pkgs.pass
    pkgs.gnupg
    pkgs.util-linux # flock
    pkgs.libnotify
  ];
in {
  home-manager.users.imalison = {
    systemd.user.services.mova-capture-drain = {
      Unit.Description = "Drain queued mova/org-agenda-api quick-captures";
      Service = {
        Type = "oneshot";
        Environment = ["PATH=${drainPath}"];
        ExecStart = "${pkgs.bash}/bin/bash ${drain}";
      };
    };

    # Retry loop: drain is idempotent and exits immediately when the queue is
    # empty, so a steady tick is cheap and covers the offline/reboot case.
    systemd.user.timers.mova-capture-drain = {
      Unit.Description = "Retry queued mova quick-captures";
      Timer = {
        OnBootSec = "2min";
        OnUnitInactiveSec = "2min";
      };
      Install.WantedBy = ["timers.target"];
    };

    # Instant drain when the front-end enqueues a job.
    systemd.user.paths.mova-capture-drain = {
      Unit.Description = "Trigger mova capture drain on enqueue";
      Path.PathExistsGlob = "%h/.local/state/mova-capture/queue/*.json";
      Install.WantedBy = ["paths.target"];
    };
  };
}
