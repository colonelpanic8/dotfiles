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
      # Use the local git checkout, not a raw path snapshot, so gitignored
      # build artifacts like dist-newstyle/.worktrees/.direnv don't get copied
      # into flake-input store sources.
      url = "git+file:///home/imalison/dotfiles/dotfiles/config/taffybar/taffybar";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
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
    ...
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
        # Use the live local checkout directly so Nix picks up dirty nested-repo
        # changes while iterating on the vendored taffybar stack.
        localTaffybarCheckout = /home/imalison/dotfiles/dotfiles/config/taffybar/taffybar;

        cleanedTaffybarSource = pkgs.lib.cleanSourceWith {
          src = localTaffybarCheckout;
          filter = path: type:
            let
              relPath = pkgs.lib.removePrefix "${toString localTaffybarCheckout}/" (toString path);
              excludedTopLevel = [ ".worktrees" ".direnv" "dist" "dist-newstyle" "result" ];
              isExcluded = pkgs.lib.lists.any
                (prefix: relPath == prefix || pkgs.lib.hasPrefix "${prefix}/" relPath)
                excludedTopLevel;
            in
              pkgs.lib.cleanSourceFilter path type && !isExcluded;
        };

        localTaffybarSubdir = subdir: cleanedTaffybarSource + "/${subdir}";

        hOverrides = hself: hsuper: {
          dbus-menu =
            pkgs.haskell.lib.overrideCabal
              (hself.callCabal2nix "dbus-menu"
                (localTaffybarSubdir "packages/dbus-menu")
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
                (localTaffybarSubdir "packages/status-notifier-item")
                { })
              (_: { doCheck = false; doHaddock = false; });

          gtk-scaling-image =
            pkgs.haskell.lib.overrideCabal
              (hself.callCabal2nix "gtk-scaling-image"
                (localTaffybarSubdir "packages/gtk-scaling-image")
                { })
              (_: { doCheck = false; doHaddock = false; });

          xdg-desktop-entry =
            pkgs.haskell.lib.overrideCabal
              (hself.callCabal2nix "xdg-desktop-entry"
                (localTaffybarSubdir "packages/xdg-desktop-entry")
                { })
              (_: { doCheck = false; doHaddock = false; });

          gtk-strut =
            pkgs.haskell.lib.overrideCabal
              (hself.callCabal2nix "gtk-strut"
                (localTaffybarSubdir "packages/gtk-strut")
                { })
              (_: { doCheck = false; doHaddock = false; });

          gtk-sni-tray =
            pkgs.haskell.lib.overrideCabal
              (hself.callCabal2nix "gtk-sni-tray"
                (localTaffybarSubdir "packages/gtk-sni-tray")
                { })
              (_: { doCheck = false; doHaddock = false; });

          dbus-hslogger =
            hself.callCabal2nix "dbus-hslogger"
              (localTaffybarSubdir "packages/dbus-hslogger")
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
            alex
            cabal-install
            happy
            # ghcid ormolu implicit-hie haskell-language-server hlint
          ]) ++ [
            pkgs.gdk-pixbuf
            pkgs.librsvg
          ];
          shellHook = ''
            # GHCi loads package DLL dependencies via the runtime linker, so it
            # needs zlib on LD_LIBRARY_PATH in addition to the build-time -L flags.
            export LD_LIBRARY_PATH="${pkgs.lib.makeLibraryPath [ pkgs.zlib ]}:''${LD_LIBRARY_PATH:-}"
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
