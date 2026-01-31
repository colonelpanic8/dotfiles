{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    git-ignore-nix.url = "github:hercules-ci/gitignore.nix";
    xmonad = {
      url = "github:xmonad/xmonad";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    xmonad-contrib = {
      url = "github:IvanMalison/xmonad-contrib/withMyChanges";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.xmonad.follows = "xmonad";
    };
  };
  outputs = { self, flake-utils, nixpkgs, xmonad, xmonad-contrib, git-ignore-nix }:
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
