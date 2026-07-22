# T3 Code source, patch stack, and package overlay.
{inputs}: final: prev: let
  commandPaletteOverlapFiles = [
    "apps/web/src/components/CommandPalette.logic.test.ts"
    "apps/web/src/components/CommandPalette.logic.ts"
    "apps/web/src/components/CommandPalette.tsx"
  ];

  # PR #4257 only changes the three shared command-palette files, so its
  # complete diff is represented by the compatibility patch below. Keep its
  # live source and hash here so a bump still audits the PR itself.
  t3codePr4257 = final.fetchurl {
    url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4257.diff";
    hash = "sha256-gdSraYeSsVMdLfQoDo0jVTv+zA/d6yaBbww4h+Dh/W8=";
  };

  # PR #4277's Sidebar V2 hunks overlap the carried command-palette PRs.
  # Keep the live cumulative diff auditable while applying that file through
  # the compatibility patch below.
  t3codePr4277 = final.fetchurl {
    url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4277.diff";
    hash = "sha256-IScOrJEV40lDaYnoVTWLkyhBe+OwVUU1KdrFyBduuxA=";
  };

  # Upstream is pinned after Theo's Threads view (#4026) merged. Keep
  # the remaining patches ordered: later patches may build on earlier UI work.
  t3codePatches = [
    # Render generated images inline: t3code#3984 (head 47cff5ac5538).
    (final.fetchurl {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/3984.diff";
      hash = "sha256-b3XsDbyKcZ3ANVT2apkOQ5lVRWeiXHLvZ3q9Yb2dFU8=";
    })
    # Searchable new-thread project picker: t3code#4259 (head af5decc0526a).
    (final.fetchurl {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4259.diff";
      hash = "sha256-2IY/3RSNRkOdNb33CWQ0miC1CS5ccili0OyHnJ+7mVI=";
    })
    # Reuse the command-palette new-thread picker for Ctrl+N: t3code#4263 (head efc01388c159).
    (final.fetchpatch {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4263.diff";
      excludes = commandPaletteOverlapFiles;
      hash = "sha256-Sox1oNf6IxwzHaVvtlBqEyyZCq+r2qz8OBL4P6SMX5I=";
    })
    # Configurable add-project shortcut: t3code#4258 (head 51e185e196be).
    (final.fetchpatch {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4258.diff";
      excludes = commandPaletteOverlapFiles;
      hash = "sha256-WZ9uZphEYW/qTscW7GrxomI93nyO/4jYVDAXWoIYhus=";
    })
    # Combined shared-file changes from #4257, #4258, and #4263.
    ./patches/t3code-command-palette-prs.patch
    # OpenAI goals against main: t3code#4260 (head 4d417d5e03bc).
    (final.fetchurl {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4260.diff";
      hash = "sha256-fiUuD7OG2mgPa7jIIADMIaV4vkKXPZY9kOUD/p9Xxhg=";
    })
    # Emacs/readline editing mode: t3code#4270 (head 195ff526dd72).
    (final.fetchurl {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4270.diff";
      hash = "sha256-0zrOswuZt7DP/1gWChy+TVmRysvxkm+uWoazwFFrLJ0=";
    })
    # Keyboard-select composer controls + hold-modifier hints: t3code#4271
    # (head 2493a41feb23). Raw diff applies with fuzz after #4258/#4260.
    (final.fetchurl {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4271.diff";
      hash = "sha256-6b7N2+KaUkoMURbwZmIskWUUUnAkDcbr4sK4ZZ6EKH8=";
    })
    # Settle the open thread with Mod+Shift+Enter: t3code#4277 (head 4ac33edaf00f).
    (final.fetchpatch {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4277.diff";
      excludes = ["apps/web/src/components/SidebarV2.tsx"];
      hash = "sha256-emcNdpyE4hGdM1H5cqrCy7S2ShzK9X8EJFe5jYjDEL0=";
    })
    # Sidebar V2 combination for #4277 with #4257/#4258/#4263.
    ./patches/t3code-settle-thread-keybinding.patch
  ];

  t3codePatchedSource = final.applyPatches {
    name = "t3code-patched-main-20260722";
    src = inputs.t3code-upstream;
    patches = builtins.seq t3codePr4257 (builtins.seq t3codePr4277 t3codePatches);
  };

  t3codeUnwrapped = (prev.t3code.unwrapped.override {pnpm_10 = final.pnpm_11;}).overrideAttrs (
    finalAttrs: previousAttrs: {
      version = "0.0.29-patched-main-20260722";
      src = t3codePatchedSource;
      # Vite+ bootstraps the exact version in packageManager. Match it
      # to nixpkgs' pnpm so the task runner uses the dependency closure
      # installed offline by pnpmConfigHook.
      postPatch =
        previousAttrs.postPatch
        + ''
          substituteInPlace package.json \
            --replace-fail '"packageManager": "pnpm@11.10.0"' \
                           '"packageManager": "pnpm@${final.pnpm_11.version}"'
        '';
      # The branch's Vite+ task runner checks every declared workspace
      # and tries to install the four intentionally-unfetched mobile
      # and infrastructure workspaces. Run the same desktop dependency
      # chain directly: web -> server -> Electron shell.
      buildPhase = ''
        runHook preBuild

        pushd apps/web
        ../../node_modules/.bin/vp build
        popd

        node apps/server/scripts/cli.ts build --verbose
        node apps/desktop/scripts/build-preview-annotation-css.mjs

        pushd apps/desktop
        ../../node_modules/.bin/vp pack
        popd

        runHook postBuild
      '';
      # `pnpm vp cache clean` also invokes pnpm's workspace bootstrap;
      # the build above does not enable Vite+ task caching.
      postBuild = "";
      pnpmDeps = final.fetchPnpmDeps {
        pnpm = final.pnpm_11;
        inherit
          (finalAttrs)
          pname
          version
          src
          pnpmWorkspaces
          ;
        fetcherVersion = 4;
        hash = "sha256-bfZDQjVdT0neQYxmNB8t+XU8mbjVsAtaTi2Vms5pzxw=";
      };
    }
  );
in {
  t3code = prev.t3code.override {t3code-unwrapped = t3codeUnwrapped;};
}
