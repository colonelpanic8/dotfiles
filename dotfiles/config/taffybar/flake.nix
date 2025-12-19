{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:colonelpanic8/nixpkgs/remove-gi-gtk-hs-patch";
    xmonad.url = "github:xmonad/xmonad/master";
    taffybar = {
      url = "github:taffybar/taffybar/master";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.xmonad.follows = "xmonad";
    };
  };
  outputs = { self, flake-utils, taffybar, nixpkgs, xmonad }:
  let
    hoverlay = final: prev: hself: hsuper:
    {
      taffybar = prev.haskell.lib.overrideCabal hsuper.taffybar (oa: {
        doHaddock = false;
        doCheck = false;
        # Fix for GHC 9.4 where liftA2 is not in Prelude
        postPatch = (oa.postPatch or "") + ''
          substituteInPlace src/System/Taffybar/DBus/Client/Util.hs \
            --replace-fail "import Control.Monad (forM)" \
                           "import Control.Monad (forM)
import Control.Applicative (liftA2)"
        '';
      });
      # gi-gtk-hs patching is now handled by taffybar's fixVersionNamePackages overlay
      imalison-taffybar = prev.haskell.lib.addPkgconfigDepends (
        hself.callCabal2nix "imalison-taffybar"
        (
          final.lib.sourceByRegex ./.
          ["taffybar.hs" "imalison-taffybar.cabal"]
        )
        { }) [
          final.util-linux.dev
          final.pcre2
          final.pcre
          final.libselinux.dev
          final.libsepol.dev
          final.fribidi.out
          final.fribidi.dev
          final.libthai.dev
          final.libdatrie.dev
          final.xorg.libXdmcp.dev
          final.libxkbcommon.dev
          final.libepoxy.dev
          final.xorg.libXtst.out
        ];
    };
    defComp = { compiler = "ghc94"; };
    overlay = xmonad.lib.fromHOL hoverlay defComp;
    overlayList = [ taffybar.overlays.default overlay ];
  in flake-utils.lib.eachDefaultSystem (system:
  let pkgs = import nixpkgs { inherit system; overlays = overlayList; config.allowBroken = true; };
      hpkgs = pkgs.lib.attrsets.getAttrFromPath (xmonad.lib.hpath defComp) pkgs;
  in
  {
    devShell = hpkgs.shellFor {
      packages = p: [ p.imalison-taffybar p.taffybar ];
      nativeBuildInputs = with hpkgs; [
        cabal-install
        # ghcid ormolu implicit-hie haskell-language-server hlint
      ];
    };
    defaultPackage = hpkgs.imalison-taffybar;
  }) // {
    inherit overlay;
    overlays = {
      default = overlay;
      taffybar = taffybar.overlays.default;
    };
  } ;
}
