let
  pkgs = (import ./base.nix);
in pkgs.haskellPackages.shellFor {
  packages = _: [
    pkgs.haskellPackages.imalison-taffybar pkgs.haskellPackages.taffybar
  ];
}
