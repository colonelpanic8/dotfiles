{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    git-ignore-nix = {
      url = "github:hercules-ci/gitignore.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # Kept for compatibility with parent flakes that set `inputs.xmonad.follows`,
    # and for taffybar's own flake inputs. We don't depend on xmonad.lib here.
    xmonad = {
      url = "github:xmonad/xmonad/master";
      inputs.nixpkgs.follows = "nixpkgs";
      # xmonad's `unstable` input is another nixpkgs pin; keep it aligned too.
      inputs.unstable.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
      inputs.git-ignore-nix.follows = "git-ignore-nix";
    };
    dbus-menu = {
      url = "github:taffybar/dbus-menu";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
      inputs.git-ignore-nix.follows = "git-ignore-nix";
    };
    status-notifier-item = {
      url = "github:taffybar/status-notifier-item";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
      inputs.git-ignore-nix.follows = "git-ignore-nix";
    };
    gtk-strut = {
      url = "github:taffybar/gtk-strut";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
      inputs.git-ignore-nix.follows = "git-ignore-nix";
    };
    gtk-sni-tray = {
      url = "github:taffybar/gtk-sni-tray";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
      inputs.git-ignore-nix.follows = "git-ignore-nix";
      inputs.gtk-strut.follows = "gtk-strut";
      inputs.status-notifier-item.follows = "status-notifier-item";
      inputs.dbus-menu.follows = "dbus-menu";
    };
    # nixpkgs' dbus-hslogger is currently too old for taffybar.
    dbus-hslogger = {
      url = "github:IvanMalison/dbus-hslogger";
      flake = false;
    };
    taffybar = {
      url = "path:/home/imalison/dotfiles/dotfiles/config/taffybar/taffybar";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
      inputs.gtk-sni-tray.follows = "gtk-sni-tray";
      inputs.gtk-strut.follows = "gtk-strut";
      inputs.status-notifier-item.follows = "status-notifier-item";
      inputs.dbus-menu.follows = "dbus-menu";
      inputs.xmonad.follows = "xmonad";
      inputs.weeder-nix.inputs.pre-commit-hooks.inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = {
    self,
    flake-utils,
    taffybar,
    nixpkgs,
    xmonad,
    dbus-menu,
    status-notifier-item,
    gtk-strut,
    gtk-sni-tray,
    dbus-hslogger,
  }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            # Taffybar's flake overlay provides a set of haskell fixes we rely on.
            taffybar.overlays.default
          ];
          config.allowBroken = true;
        };

        # Exclude local worktree/build artifacts from the source we feed to
        # callCabal2nix. cleanSource alone still includes .worktrees/.
        cleanedTaffybarSource = pkgs.lib.cleanSourceWith {
          src = taffybar.outPath;
          filter = path: type:
            let
              relPath = pkgs.lib.removePrefix "${toString taffybar.outPath}/" (toString path);
              excludedTopLevel = [ ".worktrees" ".direnv" "dist" "dist-newstyle" "result" ];
              isExcluded = pkgs.lib.lists.any
                (prefix: relPath == prefix || pkgs.lib.hasPrefix "${prefix}/" relPath)
                excludedTopLevel;
            in
              pkgs.lib.cleanSourceFilter path type && !isExcluded;
        };

        hOverrides = hself: hsuper: {
          dbus-menu =
            pkgs.haskell.lib.overrideCabal
              (hself.callCabal2nix "dbus-menu"
                (pkgs.lib.cleanSource (dbus-menu.outPath or dbus-menu))
                { inherit (pkgs) gtk3; })
              (_: {
                doCheck = false;
                doHaddock = false;
                # Needed for GHC 9.12 (template-haskell bound too strict upstream).
                jailbreak = true;
              });

          status-notifier-item =
            pkgs.haskell.lib.overrideCabal
              (hself.callCabal2nix "status-notifier-item"
                (pkgs.lib.cleanSource status-notifier-item.outPath)
                { })
              (_: { doCheck = false; doHaddock = false; });

          gtk-strut =
            pkgs.haskell.lib.overrideCabal
              (hself.callCabal2nix "gtk-strut"
                (pkgs.lib.cleanSource gtk-strut.outPath)
                { })
              (_: { doCheck = false; doHaddock = false; });

          gtk-sni-tray =
            pkgs.haskell.lib.overrideCabal
              (hself.callCabal2nix "gtk-sni-tray"
                (pkgs.lib.cleanSource gtk-sni-tray.outPath)
                { })
              (_: { doCheck = false; doHaddock = false; });

          dbus-hslogger =
            hself.callCabal2nix "dbus-hslogger"
              (pkgs.lib.cleanSource (dbus-hslogger.outPath or dbus-hslogger))
              { };

          # Build taffybar from our local flake input so it includes our extra
          # modules (e.g. System.Taffybar.Widget.ASUS) used by this config.
          taffybar = pkgs.haskell.lib.overrideCabal
            (pkgs.haskell.lib.disableStaticLibraries
              (hself.callCabal2nix "taffybar" cleanedTaffybarSource { inherit (pkgs) gtk3; }))
            (oa: {
              doHaddock = false;
              doCheck = false;
              # Legacy fix for older GHC (harmless on newer)
              postPatch = (oa.postPatch or "") + ''
                substituteInPlace src/System/Taffybar/DBus/Client/Util.hs \
                  --replace-fail "import Control.Monad (forM)" \
                                 "import Control.Monad (forM)
              import Control.Applicative (liftA2)"
              '';
              # Needed for gi-gtk-layer-shell (introspection data).
              librarySystemDepends = (oa.librarySystemDepends or []) ++ [ pkgs.gtk-layer-shell ];
            });

          # gi-gtk-hs patching is now handled by taffybar's fixVersionNamePackages overlay
          imalison-taffybar = pkgs.haskell.lib.addPkgconfigDepends (
            hself.callCabal2nix "imalison-taffybar"
              (pkgs.lib.sourceByRegex ./. [ "taffybar.hs" "imalison-taffybar.cabal" ])
              { }
          ) [
            pkgs.util-linux.dev
            pkgs.pcre2
            pkgs.pcre
            pkgs.libselinux.dev
            pkgs.libsepol.dev
            pkgs.fribidi.out
            pkgs.fribidi.dev
            pkgs.libthai.dev
            pkgs.libdatrie.dev
            pkgs.libxdmcp.dev
            pkgs.libxkbcommon.dev
            pkgs.libepoxy.dev
            pkgs.libxtst.out
          ];
        };

        # Avoid depending on xmonad.lib's helper functions, since parent flakes
        # can override the xmonad input via `follows` and change that API.
        hpkgs = pkgs.haskell.packages.ghc98.override (old: {
          overrides = pkgs.lib.composeExtensions (old.overrides or (_: _: { })) hOverrides;
        });
      in
      {
        # Expose commonly-needed ecosystem packages to callers (e.g. NixOS/home-manager
        # modules) so they can run the pinned binaries directly.
        packages = {
          status-notifier-item = hpkgs.status-notifier-item;
        };

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
      }
    ) // {
      overlays = {
        # Provide access to taffybar's overlay for callers that want it.
        taffybar = taffybar.overlays.default;
      };
    };
}
