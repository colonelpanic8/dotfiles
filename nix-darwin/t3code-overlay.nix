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
    passthru = (old.passthru or {}) // {inherit unwrapped;};
  });
}
