{
  pkgs,
  inputs,
}: let
  packages = inputs.lastfm-edit.packages.${pkgs.stdenv.hostPlatform.system};
  withSqlite = package:
    package.overrideAttrs (old: {
      buildInputs = (old.buildInputs or []) ++ [pkgs.sqlite];
    });
in {
  cli = withSqlite packages.scrobble-scrubber;
  app = withSqlite packages.scrobble-scrubber-app;
}
