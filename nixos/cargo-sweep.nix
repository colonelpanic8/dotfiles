{
  config,
  pkgs,
  ...
}: let
  dotfilesRoot = config.dotfiles-worktree;
  cargoSweepRustTargets = pkgs.writeShellApplication {
    name = "cargo-sweep-rust-targets";
    runtimeInputs = [pkgs.cargo-sweep pkgs.findutils];
    text = ''
      build_root="$HOME/.cargo/build"
      for root in "$HOME/Projects" "$HOME/org" "${dotfilesRoot}" "$build_root"; do
        if [[ -d "$root" ]]; then
          cargo-sweep sweep -r --hidden --time 2 "$root"
        fi
      done
      # Drop centralized build dirs (build_root/<xx>/<hash>) whose workspace is
      # gone or idle: cargo-sweep only removes artifacts inside them, so remove
      # any hash dir with nothing touched in the last 2 days, then empty shells.
      if [[ -d "$build_root" ]]; then
        for dir in "$build_root"/*/*/; do
          [[ -d "$dir" ]] || continue
          if [[ -z "$(find "$dir" -newermt '2 days ago' -print -quit)" ]]; then
            rm -rf "$dir"
          fi
        done
        find "$build_root" -mindepth 1 -type d -empty -delete
      fi
    '';
  };
in {
  # Defined at the system level (not home-manager) so every user gets the
  # sweep, matching the system-wide CARGO_BUILD_BUILD_DIR default.
  systemd.user.services.cargo-sweep-rust-targets = {
    description = "Sweep Rust target directories";
    serviceConfig = {
      Type = "oneshot";
      ExecStart = "${cargoSweepRustTargets}/bin/cargo-sweep-rust-targets";
    };
  };

  systemd.user.timers.cargo-sweep-rust-targets = {
    description = "Sweep Rust target directories every 6 hours";
    wantedBy = ["timers.target"];
    timerConfig = {
      OnCalendar = "00/6:00";
      Persistent = true;
      RandomizedDelaySec = "30m";
    };
  };
}
