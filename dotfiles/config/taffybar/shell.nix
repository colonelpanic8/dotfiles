let
  pkgs = (import ./base.nix);
in pkgs.haskellPackages.shellFor {
  packages = p: [ p.imalison-taffybar p.taffybar ];
}
