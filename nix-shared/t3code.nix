# T3 Code source, patch stack, and package overlay.
{inputs}: final: prev: let
  commandPaletteOverlapFiles = [
    "apps/web/src/components/CommandPalette.logic.test.ts"
    "apps/web/src/components/CommandPalette.logic.ts"
    "apps/web/src/components/CommandPalette.tsx"
  ];
  t3codePr4271OverlapFiles = [
    "apps/web/src/keybindings.test.ts"
    "packages/contracts/src/keybindings.ts"
    "packages/shared/src/keybindings.ts"
  ];
  t3codePr4277OverlapFiles = [
    "apps/web/src/components/SidebarV2.tsx"
    "apps/web/src/keybindings.test.ts"
    "packages/shared/src/keybindings.ts"
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
  t3codePr4390OverlapFiles = [
    "apps/server/src/persistence/Migrations.ts"
    "apps/web/src/components/Sidebar.tsx"
    "apps/web/src/components/SidebarV2.tsx"
  ];
  t3codePr4401OverlapFiles = [
    "apps/server/src/assets/AssetAccess.test.ts"
    "apps/server/src/assets/AssetAccess.ts"
    "apps/web/src/components/ProjectFavicon.tsx"
    "apps/web/src/components/Sidebar.logic.test.ts"
    "apps/web/src/components/SidebarV2.tsx"
  ];
  t3codePr4426OverlapFiles = [
    "apps/server/src/keybindings.test.ts"
    "apps/web/src/commandPaletteBus.ts"
    "apps/web/src/components/CommandPalette.logic.test.ts"
    "apps/web/src/components/CommandPalette.logic.ts"
    "apps/web/src/components/CommandPalette.tsx"
    "apps/web/src/environmentGrouping.test.ts"
    "apps/web/src/keybindings.test.ts"
    "packages/contracts/src/keybindings.test.ts"
    "packages/contracts/src/keybindings.ts"
    "packages/shared/src/keybindings.ts"
  ];
  t3codePr4439OverlapFiles = [
    "apps/server/src/serverSettings.test.ts"
    "packages/contracts/src/settings.ts"
  ];
  t3codePr4444OverlapFiles = [
    "apps/desktop/src/app/DesktopApp.ts"
  ];
  t3codePr4477OverlapFiles = [
    "apps/server/src/provider/Layers/ClaudeProvider.ts"
  ];

  # PR #3984 now applies directly to the pinned base. Its additional
  # artifact-safety hardening remains a separate local patch below.
  t3codePr3984 = final.fetchurl {
    url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/3984.diff";
    hash = "sha256-AewKjsqWDQ84Zy8PGIetPEueLmOa+bGSZDrJFGy6DxQ=";
  };

  # PR #4257 (head 3add2a76d8cf) only changes the three shared command-palette files, so its
  # complete diff is represented by the compatibility patch below. Keep its
  # live source and hash here so a bump still audits the PR itself.
  t3codePr4257 = final.fetchurl {
    url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4257.diff";
    hash = "sha256-2nLbkaii4O4qB/cn8c9rxyyMrNqb6TLsWVW80pea1IQ=";
  };

  # PR #4258 (head 2250aeaa6349) has command-palette hunks represented by
  # the compatibility patch below; audit its complete cumulative diff too.
  t3codePr4258 = final.fetchurl {
    url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4258.diff";
    hash = "sha256-gMbobx6gfD9CcEgSRG5UJIuCmwem8e0ahAvCk1HDlEY=";
  };

  # PR #4263 (head a5e53a9ce552) applies in full before the command-palette
  # compatibility patch combines #4257 and #4258.
  t3codePr4263 = final.fetchurl {
    url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4263.diff";
    hash = "sha256-GHEaZHjLi68G8GSikLIZdZ2dxF8X16bzOoJH30NVrc4=";
  };

  # PR #4277 (head 51a0af5abe14) overlaps Sidebar V2 and the combined
  # keybinding tests after #4263/#4271.
  # Keep the live cumulative diff auditable while applying that file through
  # the compatibility patch below.
  t3codePr4277 = final.fetchurl {
    url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4277.diff";
    hash = "sha256-AJiRBhXKmmeSOE4S+6/vvHna/FPgZGm0VVbfqlJR7Uc=";
  };

  # PR #4271 (head 1b204aa2aa62) overlaps the assembled keybinding defaults.
  t3codePr4271 = final.fetchurl {
    url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4271.diff";
    hash = "sha256-kKzEGIil44jQ/loXlyGlh5LzQVtiMGJC+/pq9Nhjm4g=";
  };

  # PR #4318 overlaps the assembled thread-route tests. Keep the live
  # cumulative diff auditable while applying that file through the
  # compatibility patch below.
  t3codePr4318 = final.fetchurl {
    url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4318.diff";
    hash = "sha256-ZUorkIlOleO5+TMqziQ/ftXnLSmNBYzZJbcrHWFW/+w=";
  };

  # PR #4324 applies in full; a small compatibility patch below updates the
  # assembled desktop settings fixture for its new default.
  t3codePr4324 = final.fetchurl {
    url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4324.diff";
    hash = "sha256-QJeB/O52Yr/CZ2CGE1uVzkxq1o9dvBWAG23fJDEQiG8=";
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

  # PR #4394 (head d23d083127fa) overlaps the composer-control stack from #4271.
  # Keep its complete cumulative diff auditable while applying the shared
  # files through the compatibility patch below.
  t3codePr4394 = final.fetchurl {
    url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4394.diff";
    hash = "sha256-WYIPc+yYUVqnNHBiPAvvfKVcVveiqj5JMeb8X+lhwoU=";
  };

  # PR #4390 overlaps the assembled legacy sidebar. Keep its complete
  # cumulative diff auditable while applying that file through the
  # compatibility patch below.
  t3codePr4390 = final.fetchurl {
    url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4390.diff";
    hash = "sha256-Llw9tPjERKTyKG2vcdd75ds6tHhteBE9O+3ExwD5q9Y=";
  };

  # PR #4401 (head 804efc44c5a8) overlaps the assembled asset and sidebar surfaces.
  # Keep its complete cumulative diff auditable while applying those
  # files through the compatibility patch below.
  t3codePr4401 = final.fetchurl {
    url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4401.diff";
    hash = "sha256-cnwdpjwHuXIVnY4Jejq41n+ktMIuskodXyBe+DEKmpg=";
  };

  # PR #4426 (head fb312224805f) overlaps the assembled command palette and
  # keybinding defaults. Keep its complete cumulative diff auditable while
  # applying those files through the compatibility patch below.
  t3codePr4426 = final.fetchurl {
    url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4426.diff";
    hash = "sha256-qHJOMAuzAEuOA4GxRnWtXVGpkWyAsa08MSz5wVaF6Ko=";
  };

  # PR #4427 (head 9939cc9a46e6) overlaps the existing generated-image and
  # asset-access stack. Keep its complete cumulative diff auditable while
  # applying the semantic combination below.
  t3codePr4427 = final.fetchurl {
    url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4427.diff";
    hash = "sha256-Z5v60/cZWdf5j5WppatoyjKJKfPAziEvBER8c6txbwI=";
  };

  # PR #4439 overlaps the assembled server-settings persistence tests. Keep
  # its complete cumulative diff auditable while applying that file through
  # the compatibility patch below.
  t3codePr4439 = final.fetchurl {
    url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4439.diff";
    hash = "sha256-j8y7AuU+E1U6O2lzy1yhIpR2hs59jNPvrsfRBwex8aU=";
  };

  # PR #4444 (head 704649ec6948) overlaps DesktopApp startup after #4437.
  t3codePr4444 = final.fetchurl {
    url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4444.diff";
    hash = "sha256-NsK02bDaCW3uprmX/dajLs1+z8UcrJKqHCtSFTUrZ8I=";
  };

  # PR #4474 is stacked on #4444. Audit its complete cumulative diff while
  # applying only the commit-pinned pairing delta after #4444 below.
  t3codePr4474 = final.fetchurl {
    url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4474.diff";
    hash = "sha256-hx/16AtAo5JpZSpXM2KftRUZ5BQNk1ZJbOnqE48MwZw=";
  };
  t3codePr4474Unique = final.fetchurl {
    url = "https://github.com/colonelpanic8/t3code/compare/704649ec6948a66e9a0b1e0199dec5a09d1e301b...8f3ac9f81d70eab58a5693d5e98e6198f428da08.diff";
    hash = "sha256-Ei+4uIsmtuhZUYaTlAl8mzuRsZqZvvXL3DegdY2R7+8=";
  };

  # PR #4477 (head bcc18e65b5b6) overlaps Claude model additions on the
  # refreshed base. Keep its complete diff auditable while applying that file
  # through the compatibility patch below.
  t3codePr4477 = final.fetchurl {
    url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4477.diff";
    hash = "sha256-DVyKYq0TTz8B9dVkFT8waGSdpz1RhaHSPo5RTwW9kLU=";
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
    t3codePr4426
    t3codePr4427
    t3codePr4439
    t3codePr4444
    t3codePr4474
    t3codePr4477
  ];

  # Upstream is pinned through branch-drift hardening (#2284). Keep the
  # remaining patches ordered: later patches may build on earlier UI work.
  t3codePatches = [
    # Render generated images inline: t3code#3984 (head 690d45fdd7a9).
    t3codePr3984
    # Constrain #3984's generic artifact paths to the real thread workspace;
    # only typed image-generation paths may use provider-managed storage.
    # Also use transactional thread snapshots, correct streaming boundaries,
    # and a bounded retry for projection/file visibility races.
    ./patches/t3code-pr-3984-artifact-safety.patch
    # Searchable new-thread project picker: t3code#4259 (head a1e3bc69a67c).
    (final.fetchurl {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4259.diff";
      hash = "sha256-sZcSRjBNjZE85LYG7znRVYYPxMozpCDVo1Z4Nuob0jg=";
    })
    # Reuse the command-palette new-thread picker for Ctrl+N:
    # t3code#4263 (head a5e53a9ce552).
    t3codePr4263
    # Configurable add-project shortcut: t3code#4258 (head 2250aeaa6349).
    (final.fetchpatch {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4258.diff";
      excludes = commandPaletteOverlapFiles;
      hash = "sha256-WZ9uZphEYW/qTscW7GrxomI93nyO/4jYVDAXWoIYhus=";
    })
    # Combine #4257 and #4258 with #4263's current command-palette flow.
    ./patches/t3code-command-palette-prs.patch
    # Keyboard-select composer controls + hold-modifier hints: t3code#4271
    # (head 1b204aa2aa62). Apply non-overlapping files directly.
    (final.fetchpatch {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4271.diff";
      excludes = t3codePr4271OverlapFiles;
      hash = "sha256-uxX7tnLMn1ghnZcqmaEmxkey22qoIGOYyocM6kA2cs4=";
    })
    ./patches/t3code-pr-4271-stack-compat.patch
    # Navigate open selections with Ctrl-N/Ctrl-P: t3code#4394
    # (head d23d083127fa). Apply its non-overlapping files directly.
    (final.fetchpatch {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4394.diff";
      excludes = t3codePr4394OverlapFiles;
      hash = "sha256-E1CrGN2OhR64kbdM4IkEtQMKwsVC5AQhQkl/FVZ6hj4=";
    })
    # Combine #4394's shared picker changes with #4271's composer controls.
    ../nixos/patches/t3code-pr-4394-stack-compat.patch
    # Settle the open thread with Mod+Shift+X: t3code#4277 (head 51a0af5abe14).
    (final.fetchpatch {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4277.diff";
      excludes = t3codePr4277OverlapFiles;
      hash = "sha256-WudBiyOsqmbQ1U9ux0MNF7HLpbBs3PUOTXkIgwOp7cU=";
    })
    # Sidebar V2 and keybinding-test combination for #4277 with #4263/#4271.
    ./patches/t3code-settle-thread-keybinding.patch
    # Recover stalled draft-thread promotion: t3code#4318 (head 2b366f5a0fe8).
    (final.fetchpatch {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4318.diff";
      excludes = ["apps/web/src/threadRoutes.test.ts"];
      hash = "sha256-57ul0B71CKYpQxKJNXuOn8XNKvqt6Bthsu7ks+DWLpw=";
    })
    ./patches/t3code-pr-4318-stack-compat.patch
    # Coalesce high-frequency assistant streaming deltas: t3code#4323
    # (head d0975d106b4f).
    (final.fetchurl {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4323.diff";
      hash = "sha256-U0x1Udkd0zJZgtwRmAV9x8djnWez6WvJAFQQ5IDiB3E=";
    })
    # Optional larger sidebar v2 project icons: t3code#4324 (head aadeaa221d09).
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
    # t3code#4332 (head e6f196d0d563).
    t3codePr4332
    # Refresh a project favicon when its icon is clicked: t3code#4337
    # (head af165bffae34).
    (final.fetchurl {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4337.diff";
      hash = "sha256-pDvYnybng8nJqFU1DiXk6hlW1kD6Exc2u7CtF5lhprI=";
    })
    # Recover stranded provider turns after lost completion events or backend
    # restarts: t3code#4386 (head 01681ab2a81b).
    (final.fetchurl {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4386.diff";
      hash = "sha256-jjrEaesVYSODYpeFfNuJ2KPUiD50Z0tJnNtyLae416g=";
    })
    # Keep the Android new-task composer above the keyboard while it expands:
    # t3code#4388 (head 7814d4798d69).
    (final.fetchurl {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4388.diff";
      hash = "sha256-nRLeyb5f8gWfVA39c1UagUOJRy8qJQ7zyi5ggEPXNN4=";
    })
    # Clone a thread through its latest completed response from the sidebar:
    # t3code#4390 (head 9ac81ba1f05a).
    (final.fetchpatch {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4390.diff";
      excludes = t3codePr4390OverlapFiles;
      hash = "sha256-FtOBL/YYAbyEYXZLrdWtLAjuDafKiDC5aqzHp6O0sDg=";
    })
    # Preserve the assembled sidebars and upstream migration registry while
    # carrying #4390's migration 35 fork-lineage entry.
    ./patches/t3code-pr-4390-stack-compat.patch
    # User-local project icons: t3code#4401 (head 804efc44c5a8).
    (final.fetchpatch {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4401.diff";
      excludes = t3codePr4401OverlapFiles;
      hash = "sha256-u/4MpS0O4/qiNW0gCwwC0abw/lmGC725tjoflha9ZQI=";
    })
    # Preserve the existing thread-artifact tests and optional large Sidebar
    # V2 icons while adding #4401's complete changes to the excluded files.
    ./patches/t3code-pr-4401-stack-compat.patch
    # Self-heal empty thread details and back off failed thread subscriptions:
    # t3code#4405 (head 6a4e6bf0b53f).
    (final.fetchurl {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4405.diff";
      hash = "sha256-nhPMr3s0kS+cW5794l+1DTENJYOD11NDLHvaOcqrOGA=";
    })
    # Closed, unmerged attempts #4419, #4420, #4421, #4423, and #4425 are
    # intentionally not carried.
    # Choose an environment, then an environment-scoped project, when creating
    # a thread with Mod+Shift+N: t3code#4426 (head fb312224805f). Apply its
    # non-overlapping files directly.
    (final.fetchpatch {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4426.diff";
      excludes = t3codePr4426OverlapFiles;
      hash = "sha256-rDqUOVI1t9YYI4fiRZl26Dkj2q06uFXVswoss9Sy+Cw=";
    })
    # Combine #4426's environment-aware thread creation flow with the
    # previously assembled command-palette and keybinding changes.
    ../nixos/patches/t3code-pr-4426-stack-compat.patch
    # Choose an environment before an environment-scoped project when creating
    # a new task on mobile: t3code#4447 (head 61fee7075d8d).
    (final.fetchurl {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4447.diff";
      hash = "sha256-LcVOyJb6I43lYcgZ3PWWEtTh4xyFuNDssNxbNL0MdYc=";
    })
    # Show completed image-generation activity as compact evidence links that
    # open in the right panel: t3code#4427 (head 9939cc9a46e6). This
    # compatibility patch keeps #3984's filename promotion, transactional
    # snapshot lookup, and artifact-path safeguards intact.
    ./patches/t3code-pr-4427-stack-compat.patch
    # Refresh positional model-picker shortcuts when virtualized rows are
    # reordered: t3code#4433 (head 6560269d0d6f).
    (final.fetchurl {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4433.diff";
      hash = "sha256-cVgAIKSWSOoXekQrwFhh8FzCVA5xRCe37zfrLihP1+A=";
    })
    # Configure centralized or repository-local worktree placement:
    # t3code#4439 (head 366d6e021814). Apply its non-overlapping files
    # directly and combine its persistence assertions with the assembled
    # server-settings tests.
    (final.fetchpatch {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4439.diff";
      excludes = t3codePr4439OverlapFiles;
      hash = "sha256-6pk81feanqE3f+tAWxVFIAkYvfVT6Pj61OGEsbdCHHw=";
    })
    ./patches/t3code-pr-4439-stack-compat.patch
    # Separate config, data, state, cache, and runtime storage with an explicit
    # XDG/legacy CLI override: t3code#4437 (head e7bae5f92d12).
    (final.fetchurl {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4437.diff";
      hash = "sha256-LJTucZOXRLHxCh9Rxu/sZObkyvEqc1Zdik4VxSV3+Qc=";
    })
    # Start Electron as a client of a separately owned local backend:
    # t3code#4444 (head 704649ec6948). Apply non-overlapping files directly.
    (final.fetchpatch {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4444.diff";
      excludes = t3codePr4444OverlapFiles;
      hash = "sha256-60qXgRNqcXBxFH/S25ARC7fEO30jQU7yYW4VB+sIeqg=";
    })
    # Combine DesktopApp startup with #4437's split cache path.
    ./patches/t3code-pr-4444-stack-compat.patch
    # Discover and explicitly pair with same-user loopback `t3 serve`
    # instances: t3code#4474 (head 8f3ac9f81d70). Its cumulative PR diff
    # includes #4444, so apply only the immutable #4444..#4474 range.
    t3codePr4474Unique
    # Discover Claude models from SDK initialization rather than relying on
    # the fallback catalog: t3code#4477 (head bcc18e65b5b6).
    (final.fetchpatch {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4477.diff";
      excludes = t3codePr4477OverlapFiles;
      hash = "sha256-t/E0DHXtKkxl8nDQcpTNr7zGJiucyt3ceF8yWv3ePhM=";
    })
    ./patches/t3code-pr-4477-stack-compat.patch
    # Refresh the sidebar resize cap when the window grows: t3code#4482
    # (head 233e5db37a7d).
    (final.fetchurl {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4482.diff";
      hash = "sha256-SmdvAW8PHJ8dpHqLmGkVsdMXxNt4A8b9DBoQx3NOmxk=";
    })
    # The snooze feature arrived with migration ID 34 after that ID had already
    # been recorded locally for ProjectionThreadGoals. Run its idempotent schema
    # update as migration 36 so existing databases receive it.
    ./patches/t3code-projection-threads-snoozed-migration-compat.patch
  ];

  t3codePatchedSource = final.applyPatches {
    name = "t3code-patched-main-20260724";
    src = inputs.t3code-upstream;
    patches = prev.lib.foldr builtins.seq t3codePatches t3codePrAudits;
    patchFlags = ["-p1" "--no-backup-if-mismatch"];
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
      postInstall =
        (previousAttrs.postInstall or "")
        + ''
          # In nixpkgs' unpacked Electron layout, app.getAppPath() resolves to
          # apps/desktop rather than the archive root. Mirror the packaged
          # app's relative renderer path for #4444's client-only mode.
          mkdir -p "$out/libexec/t3code/apps/desktop/apps/server/dist"
          ln -s ../../../../server/dist/client \
            "$out/libexec/t3code/apps/desktop/apps/server/dist/client"
        '';
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
        hash = "sha256-QNVBRvXVUOKZEdIqKY2dfjvmivMTaJJSh2cexvtdJ6k=";
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
            --add-flags "--password-store=gnome-libsecret" \
            --add-flags "--backend-mode=client-only"
        '';
    });
}
