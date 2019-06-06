let
  pkgs = import <nixpkgs> { overlays = [
    (import ./taffybar/overlay.nix) (import ../xmonad/overlay.nix)
  ]; };
  source = pkgs.lib.sourceByRegex ./. ["taffybar.hs" "imalison-taffybar.cabal"];
in pkgs.haskellPackages.callCabal2nix "imalison-taffybar" source { }
