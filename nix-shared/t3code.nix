# T3 Code package overlay.
#
# The carried patch stack used to live here as ~30 fetchurl/fetchpatch entries
# plus 19 hand-written *-stack-compat.patch conflict resolutions applied with
# `patch -p1`, together with 15 `excludes` lists naming every overlapping file.
# It is now an integration BRANCH on the fork, rebuilt from
# ../nix-shared/t3code-stack.toml by nixos/scripts/rebuild-t3code-stack.py using
# 3-way merges. See that manifest for what is carried and why, and
# ../nix-shared/t3code-stack.lock.json for exactly what the current pin contains.
#
# Everything left in this file is about BUILDING, not patching.
{inputs}: final: prev: let
  t3codeUnwrapped = (prev.t3code.unwrapped.override {pnpm_10 = final.pnpm_11;}).overrideAttrs (
    finalAttrs: previousAttrs: {
      version = "0.0.29-stack-20260725";
      src = inputs.t3code-integration;
      # Vite+ bootstraps the exact version in packageManager. Match it
      # to nixpkgs' pnpm so the task runner uses the dependency closure
      # installed offline by pnpmConfigHook.
      postPatch =
        previousAttrs.postPatch
        + ''
          substituteInPlace package.json \
            --replace-fail '"packageManager": "pnpm@11.10.0"' \
                           '"packageManager": "pnpm@${final.pnpm_11.version}"'
        '';
      # The branch's Vite+ task runner checks every declared workspace
      # and tries to install the four intentionally-unfetched mobile
      # and infrastructure workspaces. Run the same desktop dependency
      # chain directly: web -> server -> Electron shell.
      buildPhase = ''
        runHook preBuild

        pushd apps/web
        ../../node_modules/.bin/vp build
        popd

        node apps/server/scripts/cli.ts build --verbose
        node apps/desktop/scripts/build-preview-annotation-css.mjs

        pushd apps/desktop
        ../../node_modules/.bin/vp pack
        popd

        runHook postBuild
      '';
      # `pnpm vp cache clean` also invokes pnpm's workspace bootstrap;
      # the build above does not enable Vite+ task caching.
      postBuild = "";
      postInstall =
        (previousAttrs.postInstall or "")
        + ''
          # In nixpkgs' unpacked Electron layout, app.getAppPath() resolves to
          # apps/desktop rather than the archive root. Mirror the packaged
          # app's relative renderer path for #4444's client-only mode.
          mkdir -p "$out/libexec/t3code/apps/desktop/apps/server/dist"
          ln -s ../../../../server/dist/client \
            "$out/libexec/t3code/apps/desktop/apps/server/dist/client"
        '';
      pnpmDeps = final.fetchPnpmDeps {
        pnpm = final.pnpm_11;
        inherit
          (finalAttrs)
          pname
          version
          src
          pnpmWorkspaces
          ;
        fetcherVersion = 4;
        hash = "sha256-QNVBRvXVUOKZEdIqKY2dfjvmivMTaJJSh2cexvtdJ6k=";
      };
    }
  );
in {
  t3code = let
    package = prev.t3code.override {t3code-unwrapped = t3codeUnwrapped;};
  in
    package.overrideAttrs (previousAttrs: {
      buildCommand =
        previousAttrs.buildCommand
        + final.lib.optionalString final.stdenv.hostPlatform.isLinux ''
          # Chromium does not recognize Hyprland as a desktop with a native
          # password store, so Electron safeStorage otherwise selects its
          # unencrypted basic_text backend even though GNOME Keyring is
          # available through Secret Service.
          mv "$out/bin/t3code-desktop" \
            "$out/bin/.t3code-desktop-password-store-wrapped"
          makeWrapper "$out/bin/.t3code-desktop-password-store-wrapped" \
            "$out/bin/t3code-desktop" \
            --add-flags "--password-store=gnome-libsecret" \
            --add-flags "--backend-mode=client-only"
        ''
        + final.lib.optionalString final.stdenv.hostPlatform.isDarwin ''
          # Always start the packaged desktop app as a client of the
          # separately-owned t3codeServer backend (t3code#4444/#4474)
          # rather than an Electron-managed one. `makeWrapper` here is
          # makeBinaryWrapper's compiled-stub implementation (the only
          # wrapper hook symlinkJoin wires in), so this still produces a
          # native Mach-O executable at bin/t3code-desktop, which the app
          # bundle's Contents/MacOS entry symlinks to by relative path.
          mv "$out/bin/t3code-desktop" \
            "$out/bin/.t3code-desktop-client-mode-wrapped"
          makeWrapper "$out/bin/.t3code-desktop-client-mode-wrapped" \
            "$out/bin/t3code-desktop" \
            --add-flags "--backend-mode=client-only"
        '';
    });
}
