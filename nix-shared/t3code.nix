# T3 Code package overlay.
#
# The carried topics live on an integration BRANCH on the fork, rebuilt from
# ../nix-shared/t3code-stack.toml by nixos/scripts/rebuild-t3code-stack.py using
# 3-way merges. That branch carries its own flake.nix, which owns the whole
# build: pnpm pinning, the desktop build chain, the renderer symlink, and the
# dependency-closure hash.
#
# So this file does NOT rebuild the package -- it consumes the branch's flake
# output and adds the one genuinely personal thing on top: the Electron
# safeStorage wrapper. Anything build-related belongs in the fork's flake.nix,
# not here.
{inputs}: final: prev: {
  t3code =
    (inputs.t3code-integration.packages.${final.stdenv.hostPlatform.system}.t3code)
    .overrideAttrs (previousAttrs: {
      # Encode which integration rev this came from, so the store path
      # identifies the stack rather than just nixpkgs' base version.
      version = "${previousAttrs.version}-stack-${inputs.t3code-integration.shortRev}";

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
