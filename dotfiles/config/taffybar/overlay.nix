_: super: {
  haskellPackages = super.haskellPackages.override (old: {
    overrides = super.lib.composeExtensions (old.overrides or (_: _: {})) (self: _: {
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
