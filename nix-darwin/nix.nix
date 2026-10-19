{
  config,
  lib,
  pkgs,
  ...
}: {
  nix.settings = import ./nix-settings.nix;
  nix.gc = {
    automatic = true;
    interval = {
      Hour = 4;
      Minute = 15;
    };
    options = "--delete-older-than 7d";
  };
  # Root's collection only expires generations under /nix/var/nix/profiles.
  launchd.daemons.nix-gc.command = lib.mkForce "${pkgs.writeShellScript "nix-gc" ''
    for profile in /Users/*/.local/state/nix/profiles/*; do
      case "$profile" in *-link) continue ;; esac
      ${config.nix.package}/bin/nix-env --profile "$profile" --delete-generations 7d || true
    done
    exec ${config.nix.package}/bin/nix-collect-garbage ${config.nix.gc.options}
  ''}";
  nix.optimise.automatic = true;
}
