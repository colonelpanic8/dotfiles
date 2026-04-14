{
  lib,
  stdenv,
  fetchFromGitHub,
  fetchYarnDeps,
  yarnConfigHook,
  nodejs,
  makeWrapper,
}:

stdenv.mkDerivation (
  finalAttrs:
  let
    toolArchiveSuffix =
      if stdenv.hostPlatform.isLinux then
        if stdenv.hostPlatform.isAarch64 then
          "arm64-linux"
        else if stdenv.hostPlatform.isx86_64 then
          "x64-linux"
        else
          throw "Unsupported Linux architecture for happy-coder: ${stdenv.hostPlatform.system}"
      else if stdenv.hostPlatform.isDarwin then
        if stdenv.hostPlatform.isAarch64 then
          "arm64-darwin"
        else if stdenv.hostPlatform.isx86_64 then
          "x64-darwin"
        else
          throw "Unsupported Darwin architecture for happy-coder: ${stdenv.hostPlatform.system}"
      else
        throw "Unsupported platform for happy-coder: ${stdenv.hostPlatform.system}";
  in
  {
    pname = "happy-coder";
    version = "0.11.2-unstable-2026-03-26";

    src = fetchFromGitHub {
      owner = "slopus";
      repo = "happy";
      rev = "94a6bdc7b41e96b878a5ca0f8a2becdfe5a7f219";
      hash = "sha256-kcZq8raSM111wb58Uk3cyhQ5MrwtwV8zUQx+2f6kPXA=";
    };

    yarnOfflineCache = fetchYarnDeps {
      yarnLock = finalAttrs.src + "/yarn.lock";
      hash = "sha256-VjxmoOVKdOtyRAx0zVgdmLWiXzeqHVbgAXc7D3GFbc8=";
    };

    nativeBuildInputs = [
      nodejs
      yarnConfigHook
      makeWrapper
    ];

    # Fix a type mismatch in upstream TS sources.
    postPatch = ''
      substituteInPlace packages/happy-cli/src/agent/acp/runAcp.ts \
        --replace-fail 'formatOptionalDetail(mode.description,' \
                       'formatOptionalDetail(mode.description ?? undefined,'
    '';

    buildPhase = ''
      runHook preBuild

      yarn --offline workspace @slopus/happy-wire build
      yarn --offline workspace happy build

      runHook postBuild
    '';

    installPhase = ''
      runHook preInstall

      local packageOut="$out/lib/node_modules/happy-coder"
      mkdir -p "$packageOut"
      mkdir -p "$out/bin"

      cp -r packages/happy-cli/dist "$packageOut/dist"
      cp -r packages/happy-cli/bin "$packageOut/bin"
      cp -r packages/happy-cli/scripts "$packageOut/scripts"
      cp packages/happy-cli/package.json "$packageOut/package.json"

      mkdir -p "$packageOut/tools/archives" "$packageOut/tools/licenses"
      cp -r packages/happy-cli/tools/licenses/. "$packageOut/tools/licenses/"
      cp packages/happy-cli/tools/archives/difftastic-${toolArchiveSuffix}.tar.gz \
        "$packageOut/tools/archives/"
      cp packages/happy-cli/tools/archives/ripgrep-${toolArchiveSuffix}.tar.gz \
        "$packageOut/tools/archives/"

      find node_modules -mindepth 1 -maxdepth 2 -type l -delete
      cp -r node_modules "$packageOut/node_modules"

      if [ -d packages/happy-cli/node_modules ]; then
        find packages/happy-cli/node_modules -mindepth 1 -maxdepth 2 -type l -delete
        cp -rn packages/happy-cli/node_modules/. "$packageOut/node_modules/"
      fi

      mkdir -p "$packageOut/node_modules/@slopus/happy-wire"
      cp -r packages/happy-wire/dist "$packageOut/node_modules/@slopus/happy-wire/dist"
      cp packages/happy-wire/package.json "$packageOut/node_modules/@slopus/happy-wire/package.json"

      find "$packageOut/node_modules" -xtype l -delete

      for bin in happy happy-mcp; do
        makeWrapper ${nodejs}/bin/node "$out/bin/$bin" \
          --add-flags "$packageOut/bin/$bin.mjs"
      done

      runHook postInstall
    '';

    meta = {
      description = "Mobile and web client wrapper for Claude Code and Codex with end-to-end encryption";
      homepage = "https://github.com/slopus/happy";
      license = lib.licenses.mit;
      maintainers = with lib.maintainers; [ onsails ];
      mainProgram = "happy";
    };
  }
)
