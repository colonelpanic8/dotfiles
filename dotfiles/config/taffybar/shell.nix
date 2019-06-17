let
  pkgs = (import ./taffybar/nixpkgs.nix) { overlays = [
    (import ./taffybar/overlay.nix) (import ../xmonad/overlay.nix)
  ]; };
  source = pkgs.lib.sourceByRegex ./. ["taffybar.hs" "imalison-taffybar.cabal"];
  imalison-taffybar = pkgs.haskellPackages.callCabal2nix "imalison-taffybar" source { };
in pkgs.haskellPackages.shellFor { packages = _: [imalison-taffybar pkgs.haskellPackages.taffybar]; }
