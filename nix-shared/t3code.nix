# T3 Code source, patch stack, and package overlay.
{inputs}: final: prev: let
  commandPaletteOverlapFiles = [
    "apps/web/src/components/CommandPalette.logic.test.ts"
    "apps/web/src/components/CommandPalette.logic.ts"
    "apps/web/src/components/CommandPalette.tsx"
  ];

  # PR #3984 predates the pinned base. Its complete cumulative diff remains
  # auditable here while the rebased form below applies it to current main.
  t3codePr3984 = final.fetchurl {
    url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/3984.diff";
    hash = "sha256-b3XsDbyKcZ3ANVT2apkOQ5lVRWeiXHLvZ3q9Yb2dFU8=";
  };

  # PR #4257 (head c441a441f754) only changes the three shared command-palette files, so its
  # complete diff is represented by the compatibility patch below. Keep its
  # live source and hash here so a bump still audits the PR itself.
  t3codePr4257 = final.fetchurl {
    url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4257.diff";
    hash = "sha256-gHGeGzi4ss+9fUudPWc/a3lsWb3/k8fp78obdA2J31E=";
  };

  # PR #4258 (head b6dbf5d218e0) has command-palette hunks represented by
  # the compatibility patch below; audit its complete cumulative diff too.
  t3codePr4258 = final.fetchurl {
    url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4258.diff";
    hash = "sha256-FkyVXUTh+ewQHD41TnrvtzS4aUAqLnVeSDMr3cGFhwo=";
  };

  # PR #4263 (head 7851c8b7e9af) likewise has command-palette hunks in the
  # compatibility patch while its remaining files use fetchpatch below.
  t3codePr4263 = final.fetchurl {
    url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4263.diff";
    hash = "sha256-PRgyQVJ+yr8EsXBd07ueoaBavmmghnJVem/T8ASfbBo=";
  };

  # PR #4277 (head f6670ff4a82b) overlaps Sidebar V2 and the combined
  # keybinding tests after #4263/#4270/#4271.
  # Keep the live cumulative diff auditable while applying that file through
  # the compatibility patch below.
  t3codePr4277 = final.fetchurl {
    url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4277.diff";
    hash = "sha256-tRVtfhuhHl6tyTsYMXJz6Iev/hsEjarZ054ibytwg4o=";
  };

  # PR #4324 overlaps the assembled sidebar settings surface. Keep the live
  # cumulative diff auditable while applying its stack-compatible form below.
  t3codePr4324 = final.fetchurl {
    url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4324.diff";
    hash = "sha256-9cfjtpFMK7mKVKCVui0MX0mUblr0B/a70AMS+Ny9kUI=";
  };

  # GitHub's cumulative diff records binary files without their payload. Keep
  # the live PR diff auditable, then restore its exact commit-pinned PNG below.
  t3codePr4332 = final.fetchurl {
    url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4332.diff";
    hash = "sha256-AUgclGxHWoXWz6J5BLoP++JmuLdiTzIkwQrMaofQXcQ=";
  };
  t3codePr4332Foreground = final.fetchurl {
    url = "https://raw.githubusercontent.com/colonelpanic8/t3code/a5b504bc3c1394f4bb653574f06a53a57e7068fc/apps/mobile/assets/android-icon-foreground.png";
    hash = "sha256-hTFq9kDZhZPJ9odddKRuEJ3mqRzL5AnjgDIAL2WqzVc=";
  };

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
    # Searchable new-thread project picker: t3code#4259 (head 8923c66788c3).
    (final.fetchurl {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4259.diff";
      hash = "sha256-KHzjvEZkZXP4qcQ62sNWMQsBOAiKLK3pZTMV92y7G2E=";
    })
    # Reuse the command-palette new-thread picker for Ctrl+N: t3code#4263 (head 7851c8b7e9af).
    (final.fetchpatch {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4263.diff";
      excludes = commandPaletteOverlapFiles;
      hash = "sha256-XPBuI0iNXIJv6kKKJ3a10p5RBiolVWiQ8abx0Q/PF1g=";
    })
    # Configurable add-project shortcut: t3code#4258 (head b6dbf5d218e0).
    (final.fetchpatch {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4258.diff";
      excludes = commandPaletteOverlapFiles;
      hash = "sha256-WZ9uZphEYW/qTscW7GrxomI93nyO/4jYVDAXWoIYhus=";
    })
    # Combined shared-file changes from #4257, #4258, and #4263.
    ./patches/t3code-command-palette-prs.patch
    # Emacs/readline editing mode: t3code#4270 (head 25dd0ec9d0b2).
    (final.fetchurl {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4270.diff";
      hash = "sha256-ptDcxdLfoLyY2tPCBesfdXm7E/OEs9ndgGGzAar3pjo=";
    })
    # Keyboard-select composer controls + hold-modifier hints: t3code#4271
    # (head 646bd2fcc654). Raw diff applies with fuzz after #4258.
    (final.fetchurl {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4271.diff";
      hash = "sha256-KUMeja020nAvAovNfzSTv/5vRqa++3zN0YSNrtX3fIE=";
    })
    # Settle the open thread with Mod+Shift+X: t3code#4277 (head f6670ff4a82b).
    (final.fetchpatch {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4277.diff";
      excludes = [
        "apps/web/src/components/SidebarV2.tsx"
        "apps/web/src/keybindings.test.ts"
      ];
      hash = "sha256-CQWvbzxIXQhhztWQ24Q4ws5jcHDUsV3haJfJ31MMw5M=";
    })
    # Sidebar V2 and keybinding-test combination for #4277 with
    # #4263/#4270/#4271.
    ./patches/t3code-settle-thread-keybinding.patch
    # Recover stalled draft-thread promotion: t3code#4318 (head a86ea2d0bd1a).
    (final.fetchurl {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4318.diff";
      hash = "sha256-SHuHX+h3CGfSNE2RuibY+pi947x49nCGCFMov92OYmw=";
    })
    # Coalesce high-frequency assistant streaming deltas: t3code#4323
    # (head d3d98b429a50).
    (final.fetchurl {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4323.diff";
      hash = "sha256-0Y35bfZv8c4pNTkA5Gq2uKSCCDaIx9DdvmN2PHgPYdc=";
    })
    # Optional larger sidebar v2 project icons: t3code#4324 (head 88d76de90f52).
    ./patches/t3code-pr-4324-stack-compat.patch
    # Trigger the slash-command menu mid-prompt, not just at line start:
    # t3code#4181 (head d95abc106cdd).
    (final.fetchurl {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4181.diff";
      hash = "sha256-Lwjeh0D0bUKg9WX/asNArbwx8OTCx07DL75b01SzWZ0=";
    })
    # Claude provider skill discovery for the $ picker: t3code#4325
    # (head 80571c5e9f8a).
    (final.fetchurl {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4325.diff";
      hash = "sha256-1wiCnkJrB2YECXmaRFvcPtqaUifTtuvIWRSkMfEhfok=";
    })
    # Use a properly sized flat foreground for Android adaptive icons:
    # t3code#4332 (head a5b504bc3c13).
    t3codePr4332
    # Refresh a project favicon when its icon is clicked: t3code#4337
    # (head d3c12f0bc4e7).
    (final.fetchurl {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4337.diff";
      hash = "sha256-94pNY6WprLSuhVbsDMPbHIKoFQgfg8TLKFggHX/0K5k=";
    })
    # Recover stranded provider turns after lost completion events or backend
    # restarts: t3code#4386 (head 8e5b28a4dcf9).
    (final.fetchurl {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4386.diff";
      hash = "sha256-DJwvlbVaBxWHsP+DiBMDF6vjFccwfYi6a55kzkNNeQ0=";
    })
    # Fork a thread from a selected assistant response on web and mobile:
    # t3code#4390 (head 29c1ba000a58).
    (final.fetchurl {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4390.diff";
      hash = "sha256-bRZKwfj0DV1MRKgQ6gIMPeR6ZLvLkr40DAZviGq7FPI=";
    })
  ];

  t3codePatchedSource = final.applyPatches {
    name = "t3code-patched-main-20260723";
    src = inputs.t3code-upstream;
    patches =
      builtins.seq t3codePr3984
      (builtins.seq t3codePr4257
        (builtins.seq t3codePr4258
          (builtins.seq t3codePr4263
            (builtins.seq t3codePr4277
              (builtins.seq t3codePr4324
                (builtins.seq t3codePr4332 t3codePatches))))));
    postPatch = ''
      install -m 0644 ${t3codePr4332Foreground} \
        apps/mobile/assets/android-icon-foreground.png
    '';
  };

  t3codeUnwrapped = (prev.t3code.unwrapped.override {pnpm_10 = final.pnpm_11;}).overrideAttrs (
    finalAttrs: previousAttrs: {
      version = "0.0.29-patched-main-20260723";
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
