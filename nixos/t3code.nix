# T3 Code source, patch stack, and package overlay.
{inputs}: final: prev: let
  commandPaletteOverlapFiles = [
    "apps/web/src/components/CommandPalette.logic.test.ts"
    "apps/web/src/components/CommandPalette.logic.ts"
    "apps/web/src/components/CommandPalette.tsx"
  ];

  # PR #4257 (head 83ce1aa790b2) only changes the three shared command-palette files, so its
  # complete diff is represented by the compatibility patch below. Keep its
  # live source and hash here so a bump still audits the PR itself.
  t3codePr4257 = final.fetchurl {
    url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4257.diff";
    hash = "sha256-vro5lw/A5mka5pOKHKz1mNUNkifBUHBwdVUtXC6mrLA=";
  };

  # PR #4258 (head ac10e74ae628) has command-palette hunks represented by
  # the compatibility patch below; audit its complete cumulative diff too.
  t3codePr4258 = final.fetchurl {
    url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4258.diff";
    hash = "sha256-nQd8EUkV9dFR1HVjWrT1WErohYcsitBHmrvAawSoiRw=";
  };

  # PR #4263 (head 9fb451def031) likewise has command-palette hunks in the
  # compatibility patch while its remaining files use fetchpatch below.
  t3codePr4263 = final.fetchurl {
    url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4263.diff";
    hash = "sha256-gEDJ4qFt+C4oLRdun9sS4NbjNkIKBUV3q/RQTJI/+XY=";
  };

  # PR #4277 (head 2d4990a04b59) overlaps Sidebar V2 and the combined
  # keybinding tests after #4263/#4270/#4271.
  # Keep the live cumulative diff auditable while applying that file through
  # the compatibility patch below.
  t3codePr4277 = final.fetchurl {
    url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4277.diff";
    hash = "sha256-sz2yVuFYNv4kzhzA2v7Dw58IGsEQ2szbLsdLtcbfm1c=";
  };

  # PR #4318 (head 9d8c831cbd32) overlaps ChatView imports after #4271.
  # Keep the live cumulative diff auditable while applying those two files
  # through the compatibility patch below.
  t3codePr4318 = final.fetchurl {
    url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4318.diff";
    hash = "sha256-RGJYyQ61f+RbT3lD/VX6WYb5OFiuLOlHVwNxdlusmOU=";
  };

  # Upstream is pinned after Theo's Threads view (#4026) merged. Keep
  # the remaining patches ordered: later patches may build on earlier UI work.
  t3codePatches = [
    # Render generated images inline: t3code#3984 (head 47cff5ac5538).
    (final.fetchurl {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/3984.diff";
      hash = "sha256-b3XsDbyKcZ3ANVT2apkOQ5lVRWeiXHLvZ3q9Yb2dFU8=";
    })
    # Constrain #3984's generic artifact paths to the real thread workspace;
    # only typed image-generation paths may use provider-managed storage.
    ./patches/t3code-pr-3984-artifact-safety.patch
    # Searchable new-thread project picker: t3code#4259 (head 064fa4bb9fc2).
    (final.fetchurl {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4259.diff";
      hash = "sha256-8Vd/EPWMocMXANk0Rysw0zYqQgOxmMhJ3e26rs0x3cU=";
    })
    # Reuse the command-palette new-thread picker for Ctrl+N: t3code#4263 (head 9fb451def031).
    (final.fetchpatch {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4263.diff";
      excludes = commandPaletteOverlapFiles;
      hash = "sha256-Zs0GA+MfDqUibcKrd4ZyNoOGsa63akka1fqn3b6KZqU=";
    })
    # Configurable add-project shortcut: t3code#4258 (head ac10e74ae628).
    (final.fetchpatch {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4258.diff";
      excludes = commandPaletteOverlapFiles;
      hash = "sha256-WZ9uZphEYW/qTscW7GrxomI93nyO/4jYVDAXWoIYhus=";
    })
    # Combined shared-file changes from #4257, #4258, and #4263.
    ./patches/t3code-command-palette-prs.patch
    # Emacs/readline editing mode: t3code#4270 (head 4189d3ca44e6).
    (final.fetchurl {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4270.diff";
      hash = "sha256-nDHECUPDxrV84fM5WqXBYYFAqTQWcyMGB6GYZ93XPhU=";
    })
    # Keyboard-select composer controls + hold-modifier hints: t3code#4271
    # (head d0c50d2c7390). Raw diff applies with fuzz after #4258.
    (final.fetchurl {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4271.diff";
      hash = "sha256-xkv4DNBZs7EcG9OCnfk1PHBGKIB7ncDV68h0ymstXzY=";
    })
    # Settle the open thread with Mod+Shift+X: t3code#4277 (head 2d4990a04b59).
    (final.fetchpatch {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4277.diff";
      excludes = [
        "apps/web/src/components/SidebarV2.tsx"
        "apps/web/src/keybindings.test.ts"
      ];
      hash = "sha256-79ji2zIKd3jikd7UTZ+ixpSG0cRhFrTFglrrlgAHlNk=";
    })
    # Sidebar V2 and keybinding-test combination for #4277 with
    # #4263/#4270/#4271.
    ./patches/t3code-settle-thread-keybinding.patch
    # Recover stalled draft-to-server thread promotion: t3code#4318 (head
    # 9d8c831cbd32). ChatView files are combined with #4271 below.
    (final.fetchpatch {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4318.diff";
      excludes = [
        "apps/web/src/components/ChatView.logic.test.ts"
        "apps/web/src/components/ChatView.tsx"
      ];
      hash = "sha256-zUCZSYFN1M1biUcWtv9r3g3NdNtPPC0Uo0bBw+8tEKs=";
    })
    ./patches/t3code-draft-promotion-recovery.patch
  ];

  t3codePatchedSource = final.applyPatches {
    name = "t3code-patched-main-20260722";
    src = inputs.t3code-upstream;
    patches =
      builtins.seq t3codePr4257
        (builtins.seq t3codePr4258
          (builtins.seq t3codePr4263
            (builtins.seq t3codePr4277
              (builtins.seq t3codePr4318 t3codePatches))));
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
