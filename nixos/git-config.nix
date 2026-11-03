# Home Manager module: the git setup every managed user shares.
#
# ~/.gitconfig links to the repo's dotfiles/gitconfig (aliases, colors, and
# defaults), while identity and anything else per-person lives in Home
# Manager's ~/.config/git/config. Git reads ~/.gitconfig last, so the shared
# file must stay free of [user] and other per-person settings.
{
  config,
  osConfig,
  ...
}: {
  programs.git.enable = true;
  home.file.".gitconfig".source =
    config.lib.file.mkOutOfStoreSymlink "${osConfig.dotfiles-worktree}/dotfiles/gitconfig";
}
