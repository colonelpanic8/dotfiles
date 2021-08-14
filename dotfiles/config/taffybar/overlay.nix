final: prev: {
  haskellPackages = prev.haskellPackages.override (old: {
    overrides = prev.lib.composeExtensions (old.overrides or (_: _: {}))
    (hself: hsuper: {
      imalison-taffybar = hself.callCabal2nix "imalison-taffybar"
      (
        final.lib.sourceByRegex ./.
        ["taffybar.hs" "imalison-taffybar.cabal"]
      )
      { };
    });
  });
}
