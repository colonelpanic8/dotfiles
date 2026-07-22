# T3 Code source, patch stack, and package overlay.
{inputs}: final: prev: let
  # Keep this ordered: later patches may build on earlier UI work.
  t3codePatches = [
    # Theo's Threads view: t3code#4026 (head cb5791516658).
    (final.fetchurl {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4026.diff";
      hash = "sha256-adF9VNSkD0At9cCj9qRfYlYXYpM4cXfyGxwzRYKr5dU=";
    })
    # Render generated images inline: t3code#3984 (head 47cff5ac5538).
    (final.fetchurl {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/3984.diff";
      hash = "sha256-b3XsDbyKcZ3ANVT2apkOQ5lVRWeiXHLvZ3q9Yb2dFU8=";
    })
    # Searchable new-thread project picker: t3code#4259 (head 32a2bd787eef).
    (final.fetchurl {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4259.diff";
      hash = "sha256-ZZHDLIsw5aO1L9b56+pvKLxi4JJlCdQQGeJK5j6oPTU=";
    })
    # Reuse the command-palette new-thread picker for Ctrl+N: t3code#4263 (head dfd4583fd).
    (final.fetchurl {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4263.diff";
      hash = "sha256-Fb+MEkNEYzBJcOFlqc7jzmlzPyNWxlbN3Yb58MBumrw=";
    })
    # Tab completion in the directory path browser: t3code#4257 (head 324b985518d1).
    (final.fetchurl {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4257.diff";
      hash = "sha256-E7TaRgCyNXGmxwFAaLQx9wBeCz7Q1in+JCr4CwPoyoM=";
    })
    # Configurable add-project shortcut: t3code#4258 (head e7cb807d7008).
    (final.fetchurl {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4258.diff";
      hash = "sha256-LGDG/AKB2DEK22qH7YHnINbsQZhFrSJ25QjQxnwD3xc=";
    })
    # OpenAI goals against main: t3code#4260 (head 4d417d5e03bc).
    (final.fetchurl {
      url = "https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/4260.diff";
      hash = "sha256-fiUuD7OG2mgPa7jIIADMIaV4vkKXPZY9kOUD/p9Xxhg=";
    })
  ];

  t3codePatchedSource = final.applyPatches {
    name = "t3code-patched-main-20260722";
    src = inputs.t3code-upstream;
    patches = t3codePatches;
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
