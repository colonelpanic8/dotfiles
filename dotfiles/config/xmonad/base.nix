pkgs: pkgs {
  overlays = [
    (import ../taffybar/taffybar/overlay.nix)
    (import ./overlay.nix)
  ];
}
