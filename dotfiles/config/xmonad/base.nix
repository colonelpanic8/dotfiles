(import ../taffybar/taffybar/nixpkgs.nix) {
  overlays = [
    (import ../taffybar/taffybar/overlay.nix)
    (import ./overlay.nix)
  ];
}
