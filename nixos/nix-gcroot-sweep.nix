{
  config,
  pkgs,
  ...
}: let
  dotfilesRoot = config.dotfiles-worktree;
  staleDays = 90;
  sweepStaleNixGcroots = pkgs.writeShellApplication {
    name = "sweep-stale-nix-gcroots";
    runtimeInputs = [pkgs.findutils pkgs.coreutils pkgs.git];
    text = ''
      # With keep-outputs/keep-derivations on, every project .direnv and
      # result link pins its whole build closure until it is removed.
      for root in "$HOME/projects" "$HOME/org" "${dotfilesRoot}"; do
        [[ -d "$root" ]] || continue

        find "$root" -xdev \( -name node_modules -o -name .git \) -prune -o \
          -type d -name .direnv -prune -print0 |
          while IFS= read -r -d "" dir; do
            if [[ -z "$(find "$dir" -newermt '${toString staleDays} days ago' -print -quit)" ]]; then
              rm -rf -- "$dir"
            fi
          done

        find "$root" -xdev \( -name node_modules -o -name .git -o -name .direnv \) -prune -o \
          -type l -name 'result*' -lname '/nix/store/*' -mtime +${toString staleDays} -print0 |
          while IFS= read -r -d "" link; do
            if git -C "$(dirname "$link")" ls-files --error-unmatch -- "$link" >/dev/null 2>&1; then
              continue
            fi
            rm -f -- "$link"
          done
      done
    '';
  };
in {
  systemd.user.services.sweep-stale-nix-gcroots = {
    description = "Remove stale project .direnv dirs and Nix result links";
    serviceConfig = {
      Type = "oneshot";
      ExecStart = "${sweepStaleNixGcroots}/bin/sweep-stale-nix-gcroots";
    };
  };

  systemd.user.timers.sweep-stale-nix-gcroots = {
    description = "Remove stale project Nix GC roots daily";
    wantedBy = ["timers.target"];
    timerConfig = {
      OnCalendar = "daily";
      Persistent = true;
      RandomizedDelaySec = "1h";
    };
  };
}
