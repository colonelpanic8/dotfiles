pkgs: pkgs {
  overlays = [
    (import ./taffybar/overlay.nix)
    (import ../xmonad/overlay.nix)
    (import ./overlay.nix)
  ];
}
