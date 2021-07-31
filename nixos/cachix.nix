{
  nix = {
    binaryCaches = [
      "https://cache.nixos.org/"
      "https://hie-nix.cachix.org"
      "https://taffy.cachix.org"
      "https://waymonad.cachix.org"
    ];
    binaryCachePublicKeys = [
      "hie-nix.cachix.org-1:EjBSHzF6VmDnzqlldGXbi0RM3HdjfTU3yDRi9Pd0jTY="
      "taffy.cachix.org-1:H0iiOGeXnLOAptADO4i3AiDIOladTcZYxXp+P0lOvHw="
      "waymonad.cachix.org-1:Z4Nbjc3iYJNFZi0eAgzqLWFBuDRClAy1prZBv7eK64A="
    ];
  };
}
