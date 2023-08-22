{ ... }: {
  home-manager.users.imalison = {
    imports = [
      ./emacs.nix
    ];
  };
}
