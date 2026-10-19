_: prev: let
  unwrapped = prev.t3code.unwrapped.overrideAttrs (old: {
    # The two application IDs collide on case-insensitive APFS volumes.
    installPhase =
      builtins.replaceStrings
      [''ln -s t3code.png "$dir/$name.png"'']
      [''ln -sf t3code.png "$dir/$name.png"'']
      old.installPhase;
  });
in {
  t3code = prev.t3code.overrideAttrs (old: {
    paths = [unwrapped];
    buildCommand =
      old.buildCommand
      + prev.lib.optionalString prev.stdenv.hostPlatform.isDarwin ''
        for bundle in "$out"/Applications/*.app; do
          name="''${bundle##*/}"
          name="''${name%.app}"
          rm -rf "$bundle"
          cp -R ${prev.electron_44}/Applications/Electron.app "$bundle"
          chmod -R u+w "$bundle"
          cp "${unwrapped}/Applications/$name.app/Contents/Resources/t3code.icns" \
            "$bundle/Contents/Resources/t3code.icns"

          plist="$bundle/Contents/Info.plist"
          /usr/libexec/PlistBuddy -c "Set :CFBundleName $name" "$plist"
          /usr/libexec/PlistBuddy -c "Set :CFBundleDisplayName $name" "$plist"
          /usr/libexec/PlistBuddy -c "Set :CFBundleIdentifier com.t3tools.t3code.nix" "$plist"
          /usr/libexec/PlistBuddy -c "Set :CFBundleIconFile t3code.icns" "$plist"
          /usr/libexec/PlistBuddy -c "Set :CFBundleExecutable T3Code" "$plist"
          mv "$bundle/Contents/MacOS/Electron" "$bundle/Contents/MacOS/.t3code-electron"
          makeWrapper "$bundle/Contents/MacOS/.t3code-electron" "$bundle/Contents/MacOS/T3Code" \
            --prefix PATH : ${prev.lib.makeBinPath [prev.codex prev.gh prev.git]} \
            --add-flags "${unwrapped}/libexec/t3code/apps/desktop" \
            --add-flags "--backend-mode=client-only"
          /usr/bin/codesign --force --deep --sign - --preserve-metadata=entitlements "$bundle"
        done
      '';
    passthru = (old.passthru or {}) // {inherit unwrapped;};
  });
}
