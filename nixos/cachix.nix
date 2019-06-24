{
  nix = {
    binaryCaches = [
      "https://cache.nixos.org/"
      "https://hie-nix.cachix.org"
      "https://taffy.cachix.org"
    ];
    binaryCachePublicKeys = [
      "hie-nix.cachix.org-1:EjBSHzF6VmDnzqlldGXbi0RM3HdjfTU3yDRi9Pd0jTY="
      "taffy.cachix.org-1:H0iiOGeXnLOAptADO4i3AiDIOladTcZYxXp+P0lOvHw="
    ];
    trustedUsers = [ "root" "imalison" "kat" ];
  };
}
