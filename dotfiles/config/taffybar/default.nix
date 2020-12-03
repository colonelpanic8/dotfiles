let pkgs = ((import ./base.nix) (import <nixpkgs>)) ;
in
pkgs.haskellPackages.imalison-taffybar
