let
  pkgs = import <nixpkgs> { overlays = [ (import ./overlay.nix) ]; };
  source = pkgs.lib.sourceByRegex ./. [
    "xmonad.hs" "imalison-xmonad.cabal" "PagerHints.hs" "LICENSE"
  ];
in pkgs.haskellPackages.callCabal2nix "imalison-xmonad" source { }
