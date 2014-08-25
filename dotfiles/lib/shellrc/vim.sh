function vimstall() {
    # This sucks, but BundleInstall fails otherwise.
    sudo chmod -R 775 $(readlink -f ~/.vim)
    vim +BundleInstall! +q +q
}
