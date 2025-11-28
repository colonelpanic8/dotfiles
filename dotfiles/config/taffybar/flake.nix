{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:colonelpanic8/nixpkgs/remove-gi-gtk-hs-patch";
    xmonad.url = "github:xmonad/xmonad/master";
    taffybar = {
      url = "git+file:./taffybar";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.xmonad.follows = "xmonad";
    };
  };
  outputs = { self, flake-utils, taffybar, nixpkgs, xmonad }:
  let
    hoverlay = final: prev: hself: hsuper:
    let
      fixGiVersionPackage = drv:
        let
          overridden = drv.override {
            gi-gtk = hself.gi-gtk3;
            gi-gdk = hself.gi-gdk3;
          };
        in prev.haskell.lib.overrideCabal overridden (oa: {
          postPatch = (oa.postPatch or "") + ''
            substituteInPlace ${oa.pname}.cabal \
              --replace-fail "gi-gtk " "gi-gtk3 " \
              --replace-fail "gi-gdk " "gi-gdk3 "
          '';
        });
    in {
      taffybar = hsuper.taffybar.overrideAttrs (_: {
        doHaddock = false;
        doCheck = false;
      });
      gi-gtk-hs =
        let drv = hsuper.gi-gtk-hs;
        in if final.lib.versionOlder drv.version "0.3.18"
          then fixGiVersionPackage drv
          else drv;
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
