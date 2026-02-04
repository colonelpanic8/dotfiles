{ ... }: {
  home-manager.sharedModules = [
    { services.kbfs.enable = true; }
  ];
}
