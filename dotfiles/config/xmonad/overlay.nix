_: pkgs: rec {
  haskellPackages = pkgs.haskellPackages.override (old: {
    overrides = pkgs.lib.composeExtensions (old.overrides or (_: _: {})) (self: super: rec {
      imalison-xmonad = self.callCabal2nix "imalison-xmonad" (
        pkgs.lib.sourceByRegex ./.
        [
          "xmonad.hs" "imalison-xmonad.cabal"
          "PagerHints.hs" "LICENSE"
        ]
      ) { };
    });
  });
}
