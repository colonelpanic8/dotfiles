# Generic builder for Paseo plugins as Nix store directory sources.
#
# The daemon compiles plugins with esbuild `bundle: true`, resolving
# third-party imports (e.g. prismjs) from the plugin directory's own
# node_modules. So the derivation pins the plugin source with fetchgit,
# resolves its npm dependencies in the sandbox, and installs a directory
# containing the manifest, runtime entries, runtime dirs, and node_modules.
# The result is referenced as `{source = "directory", path = ...}` in the
# daemon config -- no daemon-side git clone, network, or npm at runtime.
{
  buildNpmPackage,
  lib,
  writeText,
  src,
  pname,
  version,
  npmDepsHash,
}: let
  # Upstream plugin lockfiles can lag package.json (new devDeps added without
  # regenerating the lock; `npm ci` then wants the network). Prune
  # devDependencies down to what the lock actually contains so the offline
  # install succeeds. Only devDependencies are pruned: those are tooling and
  # types by convention, while host-provided SDK imports (@getpaseo/*) are
  # externalized by the daemon's bundler and never resolve from node_modules.
  # A runtime `dependencies` entry missing from the lock still fails loudly.
  packageJson = builtins.fromJSON (builtins.readFile "${src}/package.json");
  lockJson = builtins.fromJSON (builtins.readFile "${src}/package-lock.json");
  lockedTopLevel =
    let
      names = builtins.attrNames (lockJson.packages or {});
      top = builtins.filter (n: builtins.match "node_modules/[^/]+" n != null) names;
    in
      builtins.map (n: builtins.substring 13 (builtins.stringLength n - 13) n) top;
  prunedPackageJson = writeText "paseo-plugin-package.json" (builtins.toJSON (packageJson
    // {
      devDependencies = lib.filterAttrs (name: _: builtins.elem name lockedTopLevel) (
        packageJson.devDependencies or {}
      );
    }));
in
  buildNpmPackage {
    inherit pname version src npmDepsHash;

    # Plugin repos have no `build` script; dependency resolution for the
    # daemon's bundler is all we need out of this derivation.
    dontNpmBuild = true;

    prePatch = ''
      cp ${prunedPackageJson} package.json
    '';

    installPhase = ''
    runHook preInstall
    mkdir -p "$out"
    # Manifest and package metadata the daemon reads.
    cp -a package.json paseo-plugin.json "$out/"
    # Runtime entries and dirs (v0.8 layout); copy whichever exist so one
    # builder serves client-only, server-only, and mixed plugins.
    for f in tsconfig.json index.client.tsx index.server.ts; do
      [ -e "$f" ] && cp -a "$f" "$out/"
    done
    for d in client server shared; do
      [ -d "$d" ] && cp -a "$d" "$out/"
    done
    # Resolved dependencies for esbuild bundling. Kept as a store symlink
    # when possible so outputs share the npm-deps closure.
    cp -a node_modules "$out/"
    runHook postInstall
  '';

  meta = {
    description = "Paseo plugin ${pname} as a Nix store directory source";
    license = lib.licenses.mit;
    platforms = lib.platforms.all;
  };
}
