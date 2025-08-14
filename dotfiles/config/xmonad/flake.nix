{
  inputs = {
    flake-utils.url = github:numtide/flake-utils;
    xmonad-contrib = {
      url = path:./xmonad-contrib;
      inputs.nixpkgs.follows = "nixpkgs";
    };
    xmonad = {
      url = path:./xmonad;
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = { self, flake-utils, nixpkgs, xmonad, xmonad-contrib }:
  let
    overlay = import ./overlay.nix;
    overlays = [ overlay xmonad.overlay xmonad-contrib.overlay ];
  in flake-utils.lib.eachDefaultSystem (system:
  let pkgs = import nixpkgs { inherit system overlays; config.allowBroken = true; };
  in
  {
    devShell = pkgs.haskellPackages.shellFor {
      packages = p: [ p.imalison-xmonad p.xmonad-contrib ];
      buildInputs = with pkgs.haskellPackages; [
        cabal-install haskell-language-server hlint ghcid ormolu implicit-hie
      ];
    };
    defaultPackage = pkgs.haskellPackages.imalison-xmonad;
  }) // { inherit overlay overlays; } ;
}
