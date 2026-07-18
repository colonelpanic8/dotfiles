{
  inputs = {
    taffybar = {
      # Use the vendored checkout so normal NixOS rebuilds consume the same
      # taffybar tree used for local development.
      url = "git+file:///srv/dotfiles/dotfiles/config/taffybar/taffybar";
      inputs.weeder-nix = {
        url = "github:NorfairKing/weeder-nix";
        inputs.pre-commit-hooks.inputs.nixpkgs.follows = "nixpkgs";
      };
    };
    # Follow the vendored taffybar flake's pins so the config shell and the
    # library shell mostly share their nixpkgs/Haskell dependency graph.
    flake-utils.follows = "taffybar/flake-utils";
    nixpkgs.follows = "taffybar/nixpkgs";
    # Kept for compatibility with parent flakes that set `inputs.xmonad.follows`.
    # The config flake itself does not use xmonad.lib.
    xmonad = {
      follows = "taffybar/xmonad";
    };
  };
  outputs = {
    self,
    flake-utils,
    taffybar,
    nixpkgs,
    xmonad,
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

        taffybarSourceRoot = taffybar.outPath;

        cleanedTaffybarSource = pkgs.lib.cleanSourceWith {
          src = taffybarSourceRoot;
          filter = path: type:
            let
              relPath = pkgs.lib.removePrefix "${toString taffybarSourceRoot}/" (toString path);
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

          gi-wireplumber =
            pkgs.haskell.lib.overrideCabal
              (hself.callCabal2nix "gi-wireplumber"
                (localTaffybarSubdir "packages/gi-wireplumber")
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
              (hself.callCabal2nix "taffybar" cleanedTaffybarSource {
                inherit (pkgs) gtk3;
              }))
            (oa: {
              doHaddock = false;
              doCheck = false;
              # Debug build: keep symbols (.symtab) and emit DWARF so SIGSEGV
              # coredumps symbolize to real function + source lines instead of
              # stripped hex. This library is statically linked into the
              # imalison-taffybar executable, so its symbols flow through to the
              # final binary. Revert (drop dontStrip + -g3) once the recurring
              # GC/heap-corruption crash is root-caused.
              dontStrip = true;
              configureFlags = (oa.configureFlags or []) ++ [
                "--ghc-option=-g3"
              ];
              # Needed for gi-gtk-layer-shell and gi-wireplumber introspection data.
              librarySystemDepends = (oa.librarySystemDepends or []) ++ [
                pkgs.gtk-layer-shell
                pkgs.wireplumber
              ];
              shellHook = ''
                ${oa.shellHook or ""}
                export PKG_CONFIG_PATH="${pkgs.wireplumber.dev}/lib/pkgconfig:${pkgs.pipewire.dev}/lib/pkgconfig:''${PKG_CONFIG_PATH:-}"
                export GI_GIR_PATH="${pkgs.wireplumber.dev}/share/gir-1.0:''${GI_GIR_PATH:-}"
                export GI_TYPELIB_PATH="${pkgs.wireplumber}/lib/girepository-1.0:${pkgs.glib.out}/lib/girepository-1.0:''${GI_TYPELIB_PATH:-}"
                export XDG_DATA_DIRS="${pkgs.wireplumber.dev}/share:''${XDG_DATA_DIRS:-}"
              '';
            });

          # gi-gtk-hs patching is now handled by taffybar's fixVersionNamePackages overlay
          imalison-taffybar = pkgs.haskell.lib.overrideCabal
            (pkgs.haskell.lib.addPkgconfigDepends (
              hself.callCabal2nix "imalison-taffybar"
                (pkgs.lib.sourceByRegex ./. [
                  "taffybar.hs"
                  "imalison-taffybar.cabal"
                  "TaffybarConfig"
                  "TaffybarConfig/.*"
                ])
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
            ])
            (oa: {
              # Debug build: keep the executable's .symtab (so statically
              # linked RTS + taffybar symbols resolve in coredumps) and emit
              # DWARF line info. See the taffybar override above; revert both
              # once the recurring GC/heap-corruption SIGSEGV is root-caused.
              dontStrip = true;
              configureFlags = (oa.configureFlags or []) ++ [
                "--ghc-option=-optl-fuse-ld=bfd"
                "--ld-option=-fuse-ld=bfd"
                "--with-ld=ld.bfd"
                "--ghc-option=-g3"
              ];
            });
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
            ${hpkgs.taffybar.env.shellHook or ""}
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
