let
  pkgs = (import ./base.nix (import <nixpkgs>));
in pkgs.haskellPackages.shellFor { packages = p: [ p.imalison-xmonad ]; }
