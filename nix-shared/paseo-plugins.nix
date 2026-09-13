# Declarative Paseo plugins.
#
# Each plugin is a Nix store directory source (see
# ./packages/paseo-plugin.nix): pinned by fetchgit rev + npmDepsHash, so
# `nixos-rebuild` builds it and the daemon never clones, fetches, or runs
# npm at runtime. `daemonSettings` merges into the existing
# ensure-paseo-daemon-settings merge in nixos/paseo.nix -- declared keys win,
# everything else in config.json survives.
#
# To add a plugin: append a `pkgs.callPackage ./packages/paseo-plugin.nix`
# entry, pin its rev, get the fetch hash with
#   nix run nixpkgs#nix-prefetch-git -- --url <url> --rev <rev>
# get npmDepsHash with
#   nix run nixpkgs#prefetch-npm-deps -- <src>/<pluginPath>/package-lock.json
# and add its `{source = "directory", path, enabled}` entry below.
{pkgs}: let
  mkPaseoPlugin = args: pkgs.callPackage ./packages/paseo-plugin.nix args;

  # https://paseo.cafe/plugins/colorful-agent-activity/ -- dense IDE-style
  # agent activity rows. Pinned to the paseo.cafe-scanned commit; the
  # listing's default install tracks main.
  colorful-agent-activity-src = pkgs.fetchgit {
    url = "https://github.com/mcowger/paseo-plugins";
    rev = "204dba7ac8fa89720c730949a76821e3779d2ba3";
    hash = "sha256-FU2wq8ChOIpZli6rdAqLJCFQy52NfuKMXQLaGMzlwBw=";
  };

  colorful-agent-activity = mkPaseoPlugin {
    pname = "colorful-agent-activity";
    version = "0.1.0";
    src = "${colorful-agent-activity-src}/colorful-agent-activity";
    npmDepsHash = "sha256-q1rgL02sS/VZppRG0CIgyX0GSEzi7T9omOvf9dC1L5s=";
  };
in {
  plugins = {
    inherit colorful-agent-activity;
  };

  daemonSettings = {
    pluginsEnabled = true;
    plugins = {
      "colorful-agent-activity" = {
        source = "directory";
        path = "${colorful-agent-activity}";
        enabled = true;
      };
    };
  };
}
