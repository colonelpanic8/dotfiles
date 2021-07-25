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
      coinbase-pro = hself.callCabal2nix "coinbase-pro" (final.fetchFromGitHub {
        owner = "IvanMalison";
        repo = "coinbase-pro";
        rev = "8ac93b7905150c8cbd6957102a730ecceb8b4dba";
        sha256 = "0v0xw593xczvvalh24bz37v2zbfz92dhz71f04m08abgphqmjvxq";
      }) { };
    });
  });
}
