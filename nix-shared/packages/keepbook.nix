{
  pkgs,
  inputs,
}: let
  packages = inputs.keepbook.packages.${pkgs.stdenv.hostPlatform.system};
  withLibgit2 = package:
    package.overrideAttrs (old: {
      buildInputs = (old.buildInputs or []) ++ [pkgs.libgit2];
      postInstall =
        (old.postInstall or "")
        + pkgs.lib.optionalString pkgs.stdenv.hostPlatform.isLinux ''
          wrapProgram "$out/bin/${old.pname}" \
            --prefix LD_LIBRARY_PATH : ${pkgs.lib.makeLibraryPath [pkgs.libgit2]}
        '';
    });
in {
  cli = (withLibgit2 packages.keepbook).overrideAttrs (_: {
    # Upstream checks currently depend on TS artifacts that are not built in Nix.
    doCheck = false;
  });
  desktop = withLibgit2 packages.keepbook-dioxus-desktop;
}
