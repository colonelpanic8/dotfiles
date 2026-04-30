_: pkgs: {
  haskellPackages = pkgs.haskellPackages.override (old: {
    overrides = pkgs.lib.composeExtensions (old.overrides or (_: _: {})) (self: _super: {
      imalison-river-xmonad = self.callCabal2nix "imalison-river-xmonad" (
        pkgs.lib.sourceByRegex ./.
        [
          "Main.hs"
          "imalison-river-xmonad.cabal"
        ]
      ) { };
    });
  });
}
