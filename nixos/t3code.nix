# T3 Code source, patch stack, and package overlay.
{inputs}: final: prev: let
  commandPaletteOverlapFiles = [
    "apps/web/src/components/CommandPalette.logic.test.ts"
    "apps/web/src/components/CommandPalette.logic.ts"
    "apps/web/src/components/CommandPalette.tsx"
  ];

  # PR #4257 (head dc90a36e8bf7) only changes the three shared command-palette files, so its
  # complete diff is represented by the compatibility patch below. Keep its
  # live source and hash here so a bump still audits the PR itself.
  t3codePr4257 = final.fetchurl {
    url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4257.diff";
    hash = "sha256-JJBH3EXaGPqpNFBNO1/uKmao+X0JyL++bfl+YS1e4Vo=";
  };

  # PR #4258 (head 9f48ad00b97d) has command-palette hunks represented by
  # the compatibility patch below; audit its complete cumulative diff too.
  t3codePr4258 = final.fetchurl {
    url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4258.diff";
    hash = "sha256-niqPYL/9RYJ1WL9ahyPpq4YcMmihfxha8FNNNdH9Yqc=";
  };

  # PR #4263 (head 6e0d514c884e) likewise has command-palette hunks in the
  # compatibility patch while its remaining files use fetchpatch below.
  t3codePr4263 = final.fetchurl {
    url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4263.diff";
    hash = "sha256-X0QU015jYx8hkOCwLVWzGof9MIgyaopcIZOU8CoZwLQ=";
  };

  # PR #4277 (head 60ff41deecff) overlaps Sidebar V2 and the combined
  # keybinding tests after #4263/#4270/#4271.
  # Keep the live cumulative diff auditable while applying that file through
  # the compatibility patch below.
  t3codePr4277 = final.fetchurl {
    url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4277.diff";
    hash = "sha256-VliqUb4h22oUJOtQ34w1QBzErZ5YVnuwk3bLqQNonrg=";
  };

  # Upstream is pinned through branch-drift hardening (#2284). Keep the
  # remaining patches ordered: later patches may build on earlier UI work.
  t3codePatches = [
    # Render generated images inline: t3code#3984 (head 47cff5ac5538).
    (final.fetchurl {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/3984.diff";
      hash = "sha256-b3XsDbyKcZ3ANVT2apkOQ5lVRWeiXHLvZ3q9Yb2dFU8=";
    })
    # Constrain #3984's generic artifact paths to the real thread workspace;
    # only typed image-generation paths may use provider-managed storage.
    # Also use transactional thread snapshots, correct streaming boundaries,
    # and a bounded retry for projection/file visibility races.
    ./patches/t3code-pr-3984-artifact-safety.patch
    # Searchable new-thread project picker: t3code#4259 (head b87259665220).
    (final.fetchurl {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4259.diff";
      hash = "sha256-8Vd/EPWMocMXANk0Rysw0zYqQgOxmMhJ3e26rs0x3cU=";
    })
    # Reuse the command-palette new-thread picker for Ctrl+N: t3code#4263 (head 6e0d514c884e).
    (final.fetchpatch {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4263.diff";
      excludes = commandPaletteOverlapFiles;
      hash = "sha256-RgzysvcQwmelB/3TN7AED8wPvwE43jlW1X0iT8YWGbw=";
    })
    # Configurable add-project shortcut: t3code#4258 (head 9f48ad00b97d).
    (final.fetchpatch {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4258.diff";
      excludes = commandPaletteOverlapFiles;
      hash = "sha256-WZ9uZphEYW/qTscW7GrxomI93nyO/4jYVDAXWoIYhus=";
    })
    # Combined shared-file changes from #4257, #4258, and #4263.
    ./patches/t3code-command-palette-prs.patch
    # Emacs/readline editing mode: t3code#4270 (head 85d1678228e6).
    (final.fetchurl {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4270.diff";
      hash = "sha256-LFPXt5E6RwhofXsAmdj+evKXPZMG9qMB4UDVWs3jbl0=";
    })
    # Keyboard-select composer controls + hold-modifier hints: t3code#4271
    # (head d5ca73b82391). Raw diff applies with fuzz after #4258.
    (final.fetchurl {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4271.diff";
      hash = "sha256-M5nxqZRXZdbLnXEKK+JdJaOydbnhhNg20cllI1W8f4E=";
    })
    # Settle the open thread with Mod+Shift+X: t3code#4277 (head 60ff41deecff).
    (final.fetchpatch {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4277.diff";
      excludes = [
        "apps/web/src/components/SidebarV2.tsx"
        "apps/web/src/keybindings.test.ts"
      ];
      hash = "sha256-/N2/kvKqVFh23KmOKJn0fTCLT0Wqj8TJMgg9Yk6i1pI=";
    })
    # Sidebar V2 and keybinding-test combination for #4277 with
    # #4263/#4270/#4271.
    ./patches/t3code-settle-thread-keybinding.patch
    # Recover stalled draft-thread promotion: t3code#4318 (head 9d8c831cbd32).
    (final.fetchurl {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4318.diff";
      hash = "sha256-RGJYyQ61f+RbT3lD/VX6WYb5OFiuLOlHVwNxdlusmOU=";
    })
    # Coalesce high-frequency assistant streaming deltas: t3code#4323
    # (head dbcb664e5284).
    (final.fetchurl {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4323.diff";
      hash = "sha256-4OnuT1Qz1+PdFNrLaO5jzPQVBI/uK0mSSvmh+Der/WE=";
    })
    # Optional larger sidebar v2 project icons: t3code#4324 (head e859246a4ce0).
    (final.fetchurl {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4324.diff";
      hash = "sha256-kODBBCjyhGyB98Dvtm5LHQ2msv1t/4PnYc1hlQCt0fM=";
    })
    # Trigger the slash-command menu mid-prompt, not just at line start:
    # t3code#4181 (head d95abc106cdd).
    (final.fetchurl {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4181.diff";
      hash = "sha256-Lwjeh0D0bUKg9WX/asNArbwx8OTCx07DL75b01SzWZ0=";
    })
    # Claude provider skill discovery for the $ picker: t3code#4325
    # (head 6802b6145e67).
    (final.fetchurl {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4325.diff";
      hash = "sha256-e/eaLHEgkW33qunxRvK+W2uZjpqLo/uvssxYMOVz4AY=";
    })
  ];

  t3codePatchedSource = final.applyPatches {
    name = "t3code-patched-main-20260722";
    src = inputs.t3code-upstream;
    patches =
      builtins.seq t3codePr4257
        (builtins.seq t3codePr4258
          (builtins.seq t3codePr4263
            (builtins.seq t3codePr4277 t3codePatches)));
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
