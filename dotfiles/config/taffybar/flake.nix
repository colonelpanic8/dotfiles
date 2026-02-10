{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    xmonad.url = "github:xmonad/xmonad/master";
    # Needed by gtk-sni-tray, but not (currently) provided by nixpkgs' haskellPackages.
    dbus-menu = {
      url = "github:taffybar/dbus-menu";
      flake = false;
    };
    # nixpkgs' dbus-hslogger is currently too old for taffybar.
    dbus-hslogger = {
      url = "github:IvanMalison/dbus-hslogger";
      flake = false;
    };
    taffybar = {
      url = "path:/home/imalison/dotfiles/dotfiles/config/taffybar/taffybar";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.xmonad.follows = "xmonad";
    };
  };
  outputs = { self, flake-utils, taffybar, nixpkgs, xmonad, dbus-menu, dbus-hslogger }:
  let
    hoverlay = final: prev: hself: hsuper:
    {
      dbus-menu =
        hself.callCabal2nix "dbus-menu"
        (final.lib.cleanSource dbus-menu)
        { inherit (final) gtk3; };

      dbus-hslogger =
        hself.callCabal2nix "dbus-hslogger"
        (final.lib.cleanSource dbus-hslogger)
        { };

      taffybar = prev.haskell.lib.overrideCabal hsuper.taffybar (oa: {
        doHaddock = false;
        doCheck = false;
        # Legacy fix for older GHC (harmless on newer)
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
          final.libxdmcp.dev
          final.libxkbcommon.dev
          final.libepoxy.dev
          final.libxtst.out
        ];
    };
    defComp = { compiler = "ghc98"; };
    overlay = xmonad.lib.fromHOL hoverlay defComp;
    overlayList = [ taffybar.overlays.default overlay ];
  in flake-utils.lib.eachDefaultSystem (system:
  let pkgs = import nixpkgs { inherit system; overlays = overlayList; config.allowBroken = true; };
      hpkgs = pkgs.lib.attrsets.getAttrFromPath (xmonad.lib.hpath defComp) pkgs;
  in
  {
    devShell = hpkgs.shellFor {
      packages = p: [ p.imalison-taffybar p.taffybar ];
      nativeBuildInputs = (with hpkgs; [
        cabal-install
        # ghcid ormolu implicit-hie haskell-language-server hlint
      ]) ++ [
        pkgs.gdk-pixbuf
        pkgs.librsvg
      ];
      shellHook = ''
        if [ -z "''${GDK_PIXBUF_MODULE_FILE:-}" ]; then
          export GDK_PIXBUF_MODULE_FILE="${pkgs.gdk-pixbuf}/lib/gdk-pixbuf-2.0/${pkgs.gdk-pixbuf.version}/loaders.cache"
        fi
        if [ -z "''${GDK_PIXBUF_MODULEDIR:-}" ]; then
          export GDK_PIXBUF_MODULEDIR="${pkgs.gdk-pixbuf}/lib/gdk-pixbuf-2.0/${pkgs.gdk-pixbuf.version}/loaders"
        fi
      '';
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
