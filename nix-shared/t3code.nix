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

  # PR #4257 (head ef62044f65c3) only changes the three shared command-palette files, so its
  # complete diff is represented by the compatibility patch below. Keep its
  # live source and hash here so a bump still audits the PR itself.
  t3codePr4257 = final.fetchurl {
    url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4257.diff";
    hash = "sha256-gHGeGzi4ss+9fUudPWc/a3lsWb3/k8fp78obdA2J31E=";
  };

  # PR #4258 (head d1740c6139d5) has command-palette hunks represented by
  # the compatibility patch below; audit its complete cumulative diff too.
  t3codePr4258 = final.fetchurl {
    url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4258.diff";
    hash = "sha256-FkyVXUTh+ewQHD41TnrvtzS4aUAqLnVeSDMr3cGFhwo=";
  };

  # PR #4263 (head f4d74da0c1ea) likewise has command-palette hunks in the
  # compatibility patch while its remaining files use fetchpatch below.
  t3codePr4263 = final.fetchurl {
    url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4263.diff";
    hash = "sha256-JBITmQv+ExGphqHa4EdOgtsAigTyyO125SLCbW9qG90=";
  };

  # PR #4277 (head 456bd17b31ea) overlaps Sidebar V2 and the combined
  # keybinding tests after #4263/#4271.
  # Keep the live cumulative diff auditable while applying that file through
  # the compatibility patch below.
  t3codePr4277 = final.fetchurl {
    url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4277.diff";
    hash = "sha256-/Fx879OvGib+LXiCT9vAzSfPQpUMG09dTnPBjr56NnE=";
  };

  # PR #4271 overlaps ChatView after the earlier selection-navigation patch.
  # Keep its complete cumulative diff auditable while applying that file
  # through the compatibility patch below.
  t3codePr4271 = final.fetchurl {
    url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4271.diff";
    hash = "sha256-9L5vpbUUpOZFBnNhtD4s56wa06xh3vgzRbW0TnyNLzA=";
  };

  # PR #4318 overlaps the assembled thread-route tests. Keep the live
  # cumulative diff auditable while applying that file through the
  # compatibility patch below.
  t3codePr4318 = final.fetchurl {
    url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4318.diff";
    hash = "sha256-5u4GHl1+wzxyIjUshKDHG7oK7O4CZqX/Gf+2ZYo3d3U=";
  };

  # PR #4324 overlaps the assembled sidebar settings surface. Keep the live
  # cumulative diff auditable while applying its stack-compatible form below.
  t3codePr4324 = final.fetchurl {
    url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4324.diff";
    hash = "sha256-36zRSHX6dYPh8lLnDwWd7J9kc72YvY2+EgcatiUBZAk=";
  };

  # GitHub's cumulative diff records binary files without their payload. Keep
  # the live PR diff auditable, then restore its exact commit-pinned PNG below.
  t3codePr4332 = final.fetchurl {
    url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4332.diff";
    hash = "sha256-AUgclGxHWoXWz6J5BLoP++JmuLdiTzIkwQrMaofQXcQ=";
  };
  t3codePr4332Foreground = final.fetchurl {
    url = "https://raw.githubusercontent.com/colonelpanic8/t3code/513a946d92fb582f7b74357f83af35fd2a8dedc2/apps/mobile/assets/android-icon-foreground.png";
    hash = "sha256-hTFq9kDZhZPJ9odddKRuEJ3mqRzL5AnjgDIAL2WqzVc=";
  };

  # PR #4401 overlaps the assembled asset tests and Sidebar V2 surface.
  # Keep its complete cumulative diff auditable while applying those two
  # files through the compatibility patch below.
  t3codePr4401 = final.fetchurl {
    url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4401.diff";
    hash = "sha256-TEhokBmc58cdgcWtRSvMlUdn8ZsmSGbaAjPqpu9r1hk=";
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
    t3codePr4401
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
    # Searchable new-thread project picker: t3code#4259 (head 78f2be6b6385).
    (final.fetchurl {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4259.diff";
      hash = "sha256-YsBrTop2rrOMlw5O5lv3ty+aRZ32QQBZX4+vwpSJYC4=";
    })
    # Reuse the command-palette new-thread picker for Ctrl+N: t3code#4263 (head f4d74da0c1ea).
    (final.fetchpatch {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4263.diff";
      excludes = commandPaletteOverlapFiles;
      hash = "sha256-tJ63AkFqugsVLyfiaBRo4z/4mw0vEKms4m1JrUE60Is=";
    })
    # Configurable add-project shortcut: t3code#4258 (head d1740c6139d5).
    (final.fetchpatch {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4258.diff";
      excludes = commandPaletteOverlapFiles;
      hash = "sha256-WZ9uZphEYW/qTscW7GrxomI93nyO/4jYVDAXWoIYhus=";
    })
    # Combined shared-file changes from #4257, #4258, and #4263.
    ./patches/t3code-command-palette-prs.patch
    # Navigate open selections with Ctrl-N/Ctrl-P: t3code#4394
    # (head fb36416e8f8f).
    (final.fetchurl {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4394.diff";
      hash = "sha256-PYlA1B1u0T/r7+aBfsbGgFMsuYTJ/OCt/s9W5Tb63eA=";
    })
    # Keyboard-select composer controls + hold-modifier hints: t3code#4271
    # (head 0e314b5d5ac3).
    (final.fetchpatch {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4271.diff";
      excludes = ["apps/web/src/components/ChatView.tsx"];
      hash = "sha256-GiB7B2etpjUeXbBfNFFtub95InDhSvWqHsWuX+F8b08=";
    })
    ./patches/t3code-pr-4271-stack-compat.patch
    # Settle the open thread with Mod+Shift+X: t3code#4277 (head 456bd17b31ea).
    (final.fetchpatch {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4277.diff";
      excludes = [
        "apps/web/src/components/SidebarV2.tsx"
        "apps/web/src/keybindings.test.ts"
      ];
      hash = "sha256-i+ckiUm/5oR6bBPnmJ/zNJcdKutSrmzDWxIE29ahXCI=";
    })
    # Sidebar V2 and keybinding-test combination for #4277 with #4263/#4271.
    ./patches/t3code-settle-thread-keybinding.patch
    # Recover stalled draft-thread promotion: t3code#4318 (head bcfdd61c9ff2).
    (final.fetchpatch {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4318.diff";
      excludes = ["apps/web/src/threadRoutes.test.ts"];
      hash = "sha256-JJCbOFcxjpIVPxhcrpxs5UENqcCXMm738gN8VlASawA=";
    })
    ./patches/t3code-pr-4318-stack-compat.patch
    # Coalesce high-frequency assistant streaming deltas: t3code#4323
    # (head 15097221cfc9).
    (final.fetchurl {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4323.diff";
      hash = "sha256-10GvXjgmjHGWe7k86z3gaIvoV1/kB8qc7X7aJccCeVA=";
    })
    # Optional larger sidebar v2 project icons: t3code#4324 (head 796723419898).
    ./patches/t3code-pr-4324-stack-compat.patch
    # Trigger the slash-command menu mid-prompt, not just at line start:
    # t3code#4181 (head d95abc106cdd).
    (final.fetchurl {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4181.diff";
      hash = "sha256-Lwjeh0D0bUKg9WX/asNArbwx8OTCx07DL75b01SzWZ0=";
    })
    # Claude provider skill discovery for the $ picker: t3code#4325
    # (head aac809eae519).
    (final.fetchurl {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4325.diff";
      hash = "sha256-1wiCnkJrB2YECXmaRFvcPtqaUifTtuvIWRSkMfEhfok=";
    })
    # Use a properly sized flat foreground for Android adaptive icons:
    # t3code#4332 (head 513a946d92fb).
    t3codePr4332
    # Refresh a project favicon when its icon is clicked: t3code#4337
    # (head 709407d52af5).
    (final.fetchurl {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4337.diff";
      hash = "sha256-94pNY6WprLSuhVbsDMPbHIKoFQgfg8TLKFggHX/0K5k=";
    })
    # Recover stranded provider turns after lost completion events or backend
    # restarts: t3code#4386 (head c2f43890fd73).
    (final.fetchurl {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4386.diff";
      hash = "sha256-UpjaJG1mNq+jOvNMi6huQD9I6VcNf7k7/etqvnWNci4=";
    })
    # Keep the Android new-task composer above the keyboard while it expands:
    # t3code#4388 (head 2df4224e49f0).
    (final.fetchurl {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4388.diff";
      hash = "sha256-3/BbosFzUK7soaUY+xo5Z1pFKRX8AU7n/JIT2q4FL/I=";
    })
    # Fork a thread from a selected assistant response on web and mobile:
    # t3code#4390 (head e16ab0ded616).
    (final.fetchurl {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4390.diff";
      hash = "sha256-Mjl5SohDAZQaIZ4FQi9qPuMSxg/DZqZqylpBWXzPp8I=";
    })
    # This database previously carried ProjectionThreadGoals as migration 34.
    # Keep the upstream PR unchanged while assigning its lineage migration the
    # next durable local ID in the Frankenstein stack.
    ./patches/t3code-pr-4390-stack-compat.patch
    # User-local project icons: t3code#4401 (head 012142d48fa6).
    (final.fetchpatch {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4401.diff";
      excludes = [
        "apps/server/src/assets/AssetAccess.test.ts"
        "apps/web/src/components/SidebarV2.tsx"
      ];
      hash = "sha256-3KvOqEPia15y2OTBJBkSMTIFVulio1JexTWAeDSYxnI=";
    })
    # Preserve the existing thread-artifact tests and optional large Sidebar
    # V2 icons while adding #4401's complete changes to the excluded files.
    ./patches/t3code-pr-4401-stack-compat.patch
    # Self-heal empty thread details and back off failed thread subscriptions:
    # t3code#4405 (head 4659cf54b530).
    (final.fetchurl {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4405.diff";
      hash = "sha256-nhPMr3s0kS+cW5794l+1DTENJYOD11NDLHvaOcqrOGA=";
    })
  ];

  t3codePatchedSource = final.applyPatches {
    name = "t3code-patched-main-20260723";
    src = inputs.t3code-upstream;
    patches = prev.lib.foldr builtins.seq t3codePatches t3codePrAudits;
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
