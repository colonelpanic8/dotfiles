{
  buildNpmPackage,
  lib,
  src,
}:
buildNpmPackage {
  pname = "ccusage-fleet";
  version = "0.3.0";

  inherit src;
  patches = [./ccusage-fleet-nix-fallback.patch];
  npmDepsHash = "sha256-KnrCjMFnNcF1oYNs9aUmdJsUpRmCt7RtnKymQc6ustc=";
  dontNpmBuild = true;

  # The ccusage JS launcher repairs this bit at runtime in ordinary npm
  # installs. Nix store paths are read-only, so make the bundled native binary
  # executable while the output is still writable.
  postInstall = ''
    find "$out/lib/node_modules/ccusage-fleet/node_modules/@ccusage" \
      -path '*/bin/ccusage' -exec chmod +x {} +
  '';

  meta = {
    description = "Aggregate ccusage reports across local and SSH-connected machines";
    homepage = "https://github.com/Open330/ccusage-fleet";
    license = lib.licenses.mit;
    mainProgram = "ccusage-fleet";
  };
}
