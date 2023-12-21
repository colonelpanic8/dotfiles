{
  home-manager.users.micah = {pkgs, ...}: {
    home.packages = [
      pkgs.neovim
    ];
  };
}
