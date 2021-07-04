_: super: {
  haskellPackages = super.haskellPackages.override (old: {
    overrides = super.lib.composeExtensions (old.overrides or (_: _: {})) (self: _: {
      coinbase-pro = self.callCabal2nix "coinbase-pro" (super.fetchFromGitHub {
        owner = "IvanMalison";
        repo = "coinbase-pro";
        rev = "8ac93b7905150c8cbd6957102a730ecceb8b4dba";
        sha256 = "0v0xw593xczvvalh24bz37v2zbfz92dhz71f04m08abgphqmjvxq";
      }) { };
      imalison-taffybar =
        self.callCabal2nix "imalison-taffybar"
        (
          super.lib.sourceByRegex ./.
          ["taffybar.hs" "imalison-taffybar.cabal"]
        )
        { };
    });
  });
}
