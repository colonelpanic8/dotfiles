{ forEachUser, ... }: {
  home-manager.users = forEachUser {
    services.kbfs.enable = true;
  };
}
