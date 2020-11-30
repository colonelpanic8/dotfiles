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
  packages = p: [
    (pkgs.haskell.lib.doBenchmark p.imalison-taffybar)
    (pkgs.haskell.lib.doBenchmark p.taffybar)
  ];
}
