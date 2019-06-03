_: pkgs: rec {
  haskellPackages = pkgs.haskellPackages.override (old: {
    overrides = pkgs.lib.composeExtensions (old.overrides or (_: _: {})) (self: super: rec {
      xmonad = super.xmonad.overrideAttrs (_: {
        src = fetchGit ./xmonad;
      });
      xmonad-contrib = super.xmonad-contrib.overrideAttrs (_: {
        src = fetchGit ./xmonad-contrib;
      });
    });
  });
}
