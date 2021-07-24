{
  inputs = {
    taffybar.url = path:./taffybar;
    flake-utils.url = github:numtide/flake-utils;
    git-ignore-nix.url = github:IvanMalison/gitignore.nix/master;
  };
  outputs = { self, flake-utils, taffybar, git-ignore-nix, nixpkgs }:
    let
    overlay = final: prev: {
      haskellPackages = prev.haskellPackages.override (old: {
        overrides = prev.lib.composeExtensions (old.overrides or (_: _: {}))
        (hself: hsuper: {
          imalison-taffybar = hself.callCabal2nix "imalison-taffybar"
          (git-ignore-nix.gitIgnoreSource ./.)
          { };
          coinbase-pro = hself.callCabal2nix "coinbase-pro" (final.fetchFromGitHub {
            owner = "IvanMalison";
            repo = "coinbase-pro";
            rev = "8ac93b7905150c8cbd6957102a730ecceb8b4dba";
            sha256 = "0v0xw593xczvvalh24bz37v2zbfz92dhz71f04m08abgphqmjvxq";
          }) { };
        });
      });
    };
    overlays = taffybar.overlays ++ [ overlay ];
  in flake-utils.lib.eachDefaultSystem (system:
  let pkgs = import nixpkgs { inherit system overlays; config.allowBroken = true; };
  in
  rec {
    devShell = pkgs.haskellPackages.shellFor {
      packages = p: [ p.imalison-taffybar p.taffybar ];
    };
    defaultPackage = pkgs.haskellPackages.imalison-taffybar;
  }) // { inherit overlay overlays; } ;
}
