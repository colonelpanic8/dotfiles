# scrobble-scrubber.nix - last.fm scrobble metadata cleanup daemon.
#
# Runs as a home-manager user service on a single always-on host (ryzen-shine).
# Running it on multiple hosts would mean several daemons editing the same
# last.fm account concurrently, so keep the `enable` gate to one machine.
#
# One-time bootstrap on the target host: the daemon loads a *persisted session*,
# not a password. Log in once with the `lastfm-edit` CLI (handles MFA) to write
# ~/.local/share/lastfm-edit/users/<username>/session.json before it can run.
{
  inputs,
  config,
  lib,
  ...
}: {
  home-manager.users.imalison = {...}: {
    imports = [inputs.lastfm-edit.homeManagerModules.scrobble-scrubber];

    services.scrobble-scrubber = lib.mkIf (config.networking.hostName == "ryzen-shine") {
      enable = true;
      username = "IvanMalison";
      interval = 300;
    };
  };
}
