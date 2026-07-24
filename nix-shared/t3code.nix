# T3 Code source, patch stack, and package overlay.
{inputs}: final: prev: let
  commandPaletteOverlapFiles = [
    "apps/web/src/components/CommandPalette.logic.test.ts"
    "apps/web/src/components/CommandPalette.logic.ts"
    "apps/web/src/components/CommandPalette.tsx"
  ];
  t3codePr4394OverlapFiles = [
    "apps/web/src/components/BranchToolbar.tsx"
    "apps/web/src/components/BranchToolbarEnvModeSelector.tsx"
    "apps/web/src/components/BranchToolbarEnvironmentSelector.tsx"
    "apps/web/src/components/chat/ChatComposer.tsx"
    "apps/web/src/components/chat/CompactComposerControlsMenu.tsx"
    "apps/web/src/components/chat/ProviderModelPicker.tsx"
    "apps/web/src/components/chat/TraitsPicker.tsx"
    "apps/web/src/components/chat/composerProviderState.tsx"
  ];
  t3codePr4425OverlapFiles = [
    "apps/web/src/components/Sidebar.tsx"
    "apps/web/src/components/SidebarV2.tsx"
  ];
  t3codePr4426OverlapFiles = [
    "apps/web/src/commandPaletteBus.ts"
    "apps/web/src/components/CommandPalette.logic.test.ts"
    "apps/web/src/components/CommandPalette.logic.ts"
    "apps/web/src/components/CommandPalette.tsx"
    "apps/web/src/keybindings.test.ts"
    "packages/shared/src/keybindings.ts"
  ];

  # PR #3984 predates the pinned base. Its complete cumulative diff remains
  # auditable here while the rebased form below applies it to current main.
  t3codePr3984 = final.fetchurl {
    url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/3984.diff";
    hash = "sha256-b3XsDbyKcZ3ANVT2apkOQ5lVRWeiXHLvZ3q9Yb2dFU8=";
  };

  # PR #4257 (head 8b540b72886e) only changes the three shared command-palette files, so its
  # complete diff is represented by the compatibility patch below. Keep its
  # live source and hash here so a bump still audits the PR itself.
  t3codePr4257 = final.fetchurl {
    url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4257.diff";
    hash = "sha256-2nLbkaii4O4qB/cn8c9rxyyMrNqb6TLsWVW80pea1IQ=";
  };

  # PR #4258 (head 38d2257e751a) has command-palette hunks represented by
  # the compatibility patch below; audit its complete cumulative diff too.
  t3codePr4258 = final.fetchurl {
    url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4258.diff";
    hash = "sha256-NxKNjumd3292DMFrSAhgXlgSbkzLq1Yd0fGHqorIRik=";
  };

  # PR #4263 (head 53348c731e45) likewise has command-palette hunks in the
  # compatibility patch while its remaining files use fetchpatch below.
  t3codePr4263 = final.fetchurl {
    url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4263.diff";
    hash = "sha256-HRP1g1rgb2RDaU9y3jPyJNT0jWtCgmcGspxVsXuP7pY=";
  };

  # PR #4277 (head 67ea3a70714a) overlaps Sidebar V2 and the combined
  # keybinding tests after #4263/#4271.
  # Keep the live cumulative diff auditable while applying that file through
  # the compatibility patch below.
  t3codePr4277 = final.fetchurl {
    url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4277.diff";
    hash = "sha256-tKMvuNHREMwSwkkSBqSipoB1jjY48Yb3Matg2QEvZew=";
  };

  # PR #4271 applies in full after the earlier selection-navigation patch.
  t3codePr4271 = final.fetchurl {
    url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4271.diff";
    hash = "sha256-ApHrzeMznRmF6AgX92LjaubboptJdb1c1YXy/0vp5kA=";
  };

  # PR #4318 overlaps the assembled thread-route tests. Keep the live
  # cumulative diff auditable while applying that file through the
  # compatibility patch below.
  t3codePr4318 = final.fetchurl {
    url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4318.diff";
    hash = "sha256-Buh7SAwa4reebqM4MHwAgb1NBT6w2mIl/nIoVz+X2BQ=";
  };

  # PR #4324 applies in full; a small compatibility patch below updates the
  # assembled desktop settings fixture for its new default.
  t3codePr4324 = final.fetchurl {
    url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4324.diff";
    hash = "sha256-H4RvqUdTSuxNz6UuPwQnMhGKndbFxjncmL6z3KSTWmk=";
  };

  # GitHub's cumulative diff records binary files without their payload. Keep
  # the live PR diff auditable, then restore its exact commit-pinned PNG below.
  t3codePr4332 = final.fetchurl {
    url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4332.diff";
    hash = "sha256-AUgclGxHWoXWz6J5BLoP++JmuLdiTzIkwQrMaofQXcQ=";
  };
  t3codePr4332Foreground = final.fetchurl {
    url = "https://raw.githubusercontent.com/colonelpanic8/t3code/b5839def6b09c80f1e6f4e603194ced50576a0bb/apps/mobile/assets/android-icon-foreground.png";
    hash = "sha256-hTFq9kDZhZPJ9odddKRuEJ3mqRzL5AnjgDIAL2WqzVc=";
  };

  # PR #4394 (head 5eb4ad57a3b7) overlaps the composer-control stack from #4271.
  # Keep its complete cumulative diff auditable while applying the shared
  # files through the compatibility patch below.
  t3codePr4394 = final.fetchurl {
    url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4394.diff";
    hash = "sha256-7RaVgpELDsZVokXpLxcPtazFiO7Qvp6NP076HMBPzsg=";
  };

  # PR #4390 overlaps the assembled legacy sidebar. Keep its complete
  # cumulative diff auditable while applying that file through the
  # compatibility patch below.
  t3codePr4390 = final.fetchurl {
    url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4390.diff";
    hash = "sha256-LOy1W0cGsZmF9q5e3CU078A8Pmys21hNA74qxgpJ8lU=";
  };

  # PR #4401 (head 73ce47980a9b) overlaps the assembled asset tests and Sidebar V2 surface.
  # Keep its complete cumulative diff auditable while applying those two
  # files through the compatibility patch below.
  t3codePr4401 = final.fetchurl {
    url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4401.diff";
    hash = "sha256-zxcGCbhqGaCsYmQ5Wvhr/Jm5HE/0E6C2dKkDTPJ4SXY=";
  };

  # PR #4425 (head f40fc1a0ed8a) overlaps both assembled sidebar surfaces.
  # Keep its complete cumulative diff auditable while applying those files
  # through the compatibility patch below.
  t3codePr4425 = final.fetchurl {
    url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4425.diff";
    hash = "sha256-Eh8POQGbZMUcYcDjcxJQAOcb2k70/pxzup6adTUT/bk=";
  };

  # PR #4426 (head f1dd61f7deb9) overlaps the assembled command palette and
  # keybinding defaults. Keep its complete cumulative diff auditable while
  # applying those files through the compatibility patch below.
  t3codePr4426 = final.fetchurl {
    url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4426.diff";
    hash = "sha256-CCDoLKIJeB2/MIvrJ6E+pBTWc0md63cnAamhgq0/TdQ=";
  };

  # PR #4427 (head c0a41f8cf692) overlaps the existing generated-image and
  # asset-access stack. Keep its complete cumulative diff auditable while
  # applying the semantic combination below.
  t3codePr4427 = final.fetchurl {
    url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4427.diff";
    hash = "sha256-syHIwQOW6YdHVY9AjJOeymsP28jYF1wnmuPujxJFT7U=";
  };

  t3codePrAudits = [
    t3codePr3984
    t3codePr4257
    t3codePr4258
    t3codePr4263
    t3codePr4277
    t3codePr4271
    t3codePr4318
    t3codePr4324
    t3codePr4332
    t3codePr4394
    t3codePr4390
    t3codePr4401
    t3codePr4425
    t3codePr4426
    t3codePr4427
  ];

  # Upstream is pinned through branch-drift hardening (#2284). Keep the
  # remaining patches ordered: later patches may build on earlier UI work.
  t3codePatches = [
    # Render generated images inline: t3code#3984 (head 47cff5ac5538).
    ./patches/t3code-pr-3984-main-compat.patch
    # Constrain #3984's generic artifact paths to the real thread workspace;
    # only typed image-generation paths may use provider-managed storage.
    # Also use transactional thread snapshots, correct streaming boundaries,
    # and a bounded retry for projection/file visibility races.
    ./patches/t3code-pr-3984-artifact-safety.patch
    # Searchable new-thread project picker: t3code#4259 (head 7715ef38d790).
    (final.fetchurl {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4259.diff";
      hash = "sha256-drtPerf/qpC+QxhNgJ8b7IjWbRbwxaGD+AUlkYBaCVI=";
    })
    # Reuse the command-palette new-thread picker for Ctrl+N: t3code#4263 (head 53348c731e45).
    (final.fetchpatch {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4263.diff";
      excludes = commandPaletteOverlapFiles;
      hash = "sha256-L3ffirBSu5cCCZvIKRHVZcIaBcf9+uJ4YWvVxG993OE=";
    })
    # Configurable add-project shortcut: t3code#4258 (head 38d2257e751a).
    (final.fetchpatch {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4258.diff";
      excludes = commandPaletteOverlapFiles;
      hash = "sha256-WZ9uZphEYW/qTscW7GrxomI93nyO/4jYVDAXWoIYhus=";
    })
    # Combined shared-file changes from #4257, #4258, and #4263.
    ./patches/t3code-command-palette-prs.patch
    # Keyboard-select composer controls + hold-modifier hints: t3code#4271
    # (head 32156502206a).
    t3codePr4271
    # Navigate open selections with Ctrl-N/Ctrl-P: t3code#4394
    # (head 5eb4ad57a3b7). Apply its non-overlapping files directly.
    (final.fetchpatch {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4394.diff";
      excludes = t3codePr4394OverlapFiles;
      hash = "sha256-E1CrGN2OhR64kbdM4IkEtQMKwsVC5AQhQkl/FVZ6hj4=";
    })
    # Combine #4394's shared picker changes with #4271's composer controls.
    ../nixos/patches/t3code-pr-4394-stack-compat.patch
    # Settle the open thread with Mod+Shift+X: t3code#4277 (head 67ea3a70714a).
    (final.fetchpatch {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4277.diff";
      excludes = [
        "apps/web/src/components/SidebarV2.tsx"
        "apps/web/src/keybindings.test.ts"
      ];
      hash = "sha256-fDT1ee6vX/KN89ZTjgD7Eh10ittr8ogHcEDcrnr5LNk=";
    })
    # Sidebar V2 and keybinding-test combination for #4277 with #4263/#4271.
    ./patches/t3code-settle-thread-keybinding.patch
    # Recover stalled draft-thread promotion: t3code#4318 (head b3188518cb5c).
    (final.fetchpatch {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4318.diff";
      excludes = ["apps/web/src/threadRoutes.test.ts"];
      hash = "sha256-KqkcCmGBjkxo4ZGnNafKflsh+uQhzq+peVkeslzFIa0=";
    })
    ./patches/t3code-pr-4318-stack-compat.patch
    # Coalesce high-frequency assistant streaming deltas: t3code#4323
    # (head 5d4f36e0c64f).
    (final.fetchurl {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4323.diff";
      hash = "sha256-10GvXjgmjHGWe7k86z3gaIvoV1/kB8qc7X7aJccCeVA=";
    })
    # Optional larger sidebar v2 project icons: t3code#4324 (head 2b543c77aa1d).
    t3codePr4324
    # Keep the assembled desktop settings fixture aligned with the added
    # sidebarV2LargeIcons default.
    ./patches/t3code-pr-4324-stack-compat.patch
    # Trigger the slash-command menu mid-prompt, not just at line start:
    # t3code#4181 (head d95abc106cdd).
    (final.fetchurl {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4181.diff";
      hash = "sha256-Lwjeh0D0bUKg9WX/asNArbwx8OTCx07DL75b01SzWZ0=";
    })
    # Claude provider skill discovery is upstream via t3code#4414.
    # Use a properly sized flat foreground for Android adaptive icons:
    # t3code#4332 (head b5839def6b09).
    t3codePr4332
    # Refresh a project favicon when its icon is clicked: t3code#4337
    # (head d6ea27c611e6).
    (final.fetchurl {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4337.diff";
      hash = "sha256-94pNY6WprLSuhVbsDMPbHIKoFQgfg8TLKFggHX/0K5k=";
    })
    # Recover stranded provider turns after lost completion events or backend
    # restarts: t3code#4386 (head 1a28657d833d).
    (final.fetchurl {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4386.diff";
      hash = "sha256-UpjaJG1mNq+jOvNMi6huQD9I6VcNf7k7/etqvnWNci4=";
    })
    # Keep the Android new-task composer above the keyboard while it expands:
    # t3code#4388 (head 4e3aa7b26ced).
    (final.fetchurl {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4388.diff";
      hash = "sha256-3/BbosFzUK7soaUY+xo5Z1pFKRX8AU7n/JIT2q4FL/I=";
    })
    # Clone a thread through its latest completed response from the sidebar:
    # t3code#4390 (head b9b18e0c8a13).
    (final.fetchpatch {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4390.diff";
      excludes = ["apps/web/src/components/Sidebar.tsx"];
      hash = "sha256-i9xUHbE5/bONup5/KUYgN7IbflU6GtGsiYZ+ZG+bCU8=";
    })
    # This database previously carried ProjectionThreadGoals as migration 34.
    # Keep the upstream PR unchanged while assigning its lineage migration the
    # next durable local ID in the Frankenstein stack.
    ./patches/t3code-pr-4390-stack-compat.patch
    # User-local project icons: t3code#4401 (head 73ce47980a9b).
    (final.fetchpatch {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4401.diff";
      excludes = [
        "apps/server/src/assets/AssetAccess.test.ts"
        "apps/web/src/components/SidebarV2.tsx"
      ];
      hash = "sha256-vfOthSAhatGmMoSE0xPOj2pPY4qZaKWa+uCcEhhJais=";
    })
    # Preserve the existing thread-artifact tests and optional large Sidebar
    # V2 icons while adding #4401's complete changes to the excluded files.
    ./patches/t3code-pr-4401-stack-compat.patch
    # Self-heal empty thread details and back off failed thread subscriptions:
    # t3code#4405 (head ee2ccd7a588b).
    (final.fetchurl {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4405.diff";
      hash = "sha256-nhPMr3s0kS+cW5794l+1DTENJYOD11NDLHvaOcqrOGA=";
    })
    # Honor XDG base directories for data and settings: t3code#4419
    # (head a5edd8110c12).
    (final.fetchurl {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4419.diff";
      hash = "sha256-rXFy5jqN+w3iggxZL6gFya85MEz0jZolIqA4fzbmTDU=";
    })
    # Refuse startup when another server owns the state dir, plus SQLite
    # busy_timeout: t3code#4420 (head bad5be3e2745).
    (final.fetchurl {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4420.diff";
      hash = "sha256-K/Mxja9loe1MITDFjFYxCKi/xaEwbpVp/Py26pBMeFo=";
    })
    # Settle stale running provider sessions on startup: t3code#4421
    # (head a638faf43b27).
    (final.fetchurl {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4421.diff";
      hash = "sha256-3VZ3ENQXVrEtWA2nSeSgh7RTLt9RH6u7cXQ7X+gz64A=";
    })
    # Desktop attaches to an existing local backend instead of spawning;
    # server mints the 0600 local-attach-token: t3code#4423
    # (head 2cbc91518db0).
    (final.fetchurl {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4423.diff";
      hash = "sha256-6hFncBXCNsIUp3D1WGRaUBmkqgL90aBPGYNPPNa/k7I=";
    })
    # Show remote environment names in thread rows when space permits:
    # t3code#4425 (head f40fc1a0ed8a). Apply its non-overlapping files directly.
    (final.fetchpatch {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4425.diff";
      excludes = t3codePr4425OverlapFiles;
      hash = "sha256-ADySVSYSboZm8lAMnQ4ftV10G4DdPkGmVVmLcq+PsyM=";
    })
    # Combine #4425's sidebar changes with the previously assembled sidebar
    # features from #4390 and #4401.
    ../nixos/patches/t3code-pr-4425-stack-compat.patch
    # Choose an environment, then an environment-scoped project, when creating
    # a thread with Mod+Shift+N: t3code#4426 (head f1dd61f7deb9). Apply its
    # non-overlapping files directly.
    (final.fetchpatch {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4426.diff";
      excludes = t3codePr4426OverlapFiles;
      hash = "sha256-DbuyyjSvlwT4JvalkhfiE10kKGENPB6FdIlQS69i7q4=";
    })
    # Combine #4426's environment-aware thread creation flow with the
    # previously assembled command-palette and keybinding changes.
    ../nixos/patches/t3code-pr-4426-stack-compat.patch
    # Show completed image-generation activity as compact evidence links that
    # open in the right panel: t3code#4427 (head c0a41f8cf692). This
    # compatibility patch keeps #3984's filename promotion, transactional
    # snapshot lookup, and artifact-path safeguards intact.
    ./patches/t3code-pr-4427-stack-compat.patch
  ];

  t3codePatchedSource = final.applyPatches {
    name = "t3code-patched-main-20260724";
    src = inputs.t3code-upstream;
    patches = prev.lib.foldr builtins.seq t3codePatches t3codePrAudits;
    postPatch = ''
      install -m 0644 ${t3codePr4332Foreground} \
        apps/mobile/assets/android-icon-foreground.png
    '';
  };

  t3codeUnwrapped = (prev.t3code.unwrapped.override {pnpm_10 = final.pnpm_11;}).overrideAttrs (
    finalAttrs: previousAttrs: {
      version = "0.0.29-patched-main-20260724";
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
  t3code = let
    package = prev.t3code.override {t3code-unwrapped = t3codeUnwrapped;};
  in
    package.overrideAttrs (previousAttrs: {
      buildCommand =
        previousAttrs.buildCommand
        + final.lib.optionalString final.stdenv.hostPlatform.isLinux ''
          # Chromium does not recognize Hyprland as a desktop with a native
          # password store, so Electron safeStorage otherwise selects its
          # unencrypted basic_text backend even though GNOME Keyring is
          # available through Secret Service.
          mv "$out/bin/t3code-desktop" \
            "$out/bin/.t3code-desktop-password-store-wrapped"
          makeWrapper "$out/bin/.t3code-desktop-password-store-wrapped" \
            "$out/bin/t3code-desktop" \
            --add-flags "--password-store=gnome-libsecret"
        '';
    });
}
