let
  pkgs = (import ./base.nix);
  profiledHaskellPackages = pkgs.haskellPackages.override (old: {
    overrides = pkgs.lib.composeExtensions (old.overrides or (_: _: {})) (self: super: {
      mkDerivation = args: super.mkDerivation (args // {
          enableLibraryProfiling = true;
      });
    });
  });
in pkgs.haskellPackages.shellFor {
  packages = _: [
    (pkgs.haskell.lib.doBenchmark pkgs.haskellPackages.imalison-taffybar) pkgs.haskellPackages.taffybar
  ];
}

