{
  buildPackages,
  lib,
  stdenv,
  stdenvNoCC,
  fetchFromGitHub,
  nix-update-script,
  makeDesktopItem,
  electron_40,
  nodejs,
}:

stdenv.mkDerivation (
  finalAttrs:
  let
    electron = electron_40;
    nodeModules = stdenvNoCC.mkDerivation {
      pname = "${finalAttrs.pname}-node_modules";
      inherit (finalAttrs) src version strictDeps;

      nativeBuildInputs = [
        buildPackages.bun
        buildPackages.nodejs
        buildPackages.writableTmpDirAsHomeHook
      ];

      dontConfigure = true;
      dontFixup = true;

      postPatch = ''
        replacePackageVersion() {
          local packageJson="$1"
          local currentVersion="$(sed -n 's/.*"version": "\([^"]*\)".*/\1/p' "$packageJson" | head -n1)"
          local currentVersionPattern

          printf -v currentVersionPattern '"version": "%s"' "$currentVersion"

          substituteInPlace "$packageJson" \
            --replace-fail "$currentVersionPattern" '"version": "${finalAttrs.version}"'
        }

        for packageJson in \
          apps/{desktop,server,web}/package.json \
          packages/{contracts,shared}/package.json
        do
          replacePackageVersion "$packageJson"
        done

        for packageJson in packages/{contracts,shared}/package.json; do
          substituteInPlace "$packageJson" \
            --replace-fail '"prepare": "effect-language-service patch",' '"prepare": "true",'
        done
      '';

      buildPhase = ''
        runHook preBuild

        bun install \
          --cpu="*" \
          --ignore-scripts \
          --no-progress \
          --frozen-lockfile \
          --os="*"

        bun --bun ${./canonicalize-node-modules.ts}
        bun --bun ${./normalize-bun-binaries.ts}

        runHook postBuild
      '';

      installPhase = ''
        runHook preInstall

        mkdir -p $out
        cp -r node_modules $out
        find apps packages -type d -name node_modules -exec cp -r --parents {} $out \;

        runHook postInstall
      '';

      outputHash = "sha256-yrzdhw+NPYZku10piHoxMy+TUJ8MYySZorMOMOztJY4=";
      outputHashMode = "recursive";
    };
  in
  {
    pname = "t3code";
    version = "0.0.15";
    strictDeps = true;

    src = fetchFromGitHub {
      owner = "pingdotgg";
      repo = "t3code";
      tag = "v${finalAttrs.version}";
      hash = "sha256-HOPiA8X/FzswKGmOuYKog3YIn5iq5rJ/7kDoGhN11x0=";
    };

    postPatch = ''
      replacePackageVersion() {
        local packageJson="$1"
        local currentVersion="$(sed -n 's/.*"version": "\([^"]*\)".*/\1/p' "$packageJson" | head -n1)"
        local currentVersionPattern

        printf -v currentVersionPattern '"version": "%s"' "$currentVersion"

        substituteInPlace "$packageJson" \
          --replace-fail "$currentVersionPattern" '"version": "${finalAttrs.version}"'
      }

      for packageJson in \
        apps/{desktop,server,web}/package.json \
        packages/contracts/package.json
      do
        replacePackageVersion "$packageJson"
      done

      printf -v resourcePathSearch '%s\n%s' \
        'Path.join(__dirname, "../prod-resources", fileName),' \
        '    Path.join(process.resourcesPath, "resources", fileName),'
      printf -v resourcePathReplacement '%s\n%s\n%s' \
        'Path.join(__dirname, "../prod-resources", fileName),' \
        '    Path.join(ROOT_DIR, "apps", "desktop", "prod-resources", fileName),' \
        '    Path.join(process.resourcesPath, "resources", fileName),'

      substituteInPlace apps/desktop/src/main.ts \
        --replace-fail "$resourcePathSearch" "$resourcePathReplacement"

      substituteInPlace apps/web/vite.config.ts \
        --replace-fail '  server: {' $'  server: {\n    host: "127.0.0.1",' \
        --replace-fail 'host: "localhost"' 'host: "127.0.0.1"'
    '';

    nativeBuildInputs = [
      buildPackages.bun
      buildPackages.copyDesktopItems
      buildPackages.installShellFiles
      buildPackages.makeBinaryWrapper
      buildPackages.node-gyp
      buildPackages.nodejs
      buildPackages.python3
      buildPackages.writableTmpDirAsHomeHook
    ] ++ lib.optionals stdenv.buildPlatform.isDarwin [
      buildPackages.cctools.libtool
      buildPackages.xcbuild
    ];

    nativeInstallCheckInputs = [ buildPackages.versionCheckHook ];
    doInstallCheck = stdenv.buildPlatform.canExecute stdenv.hostPlatform;

    configurePhase = ''
      runHook preConfigure

      cp -r ${nodeModules}/. .

      chmod -R u+rwX node_modules
      patchShebangs node_modules

      export npm_config_nodedir=${nodejs}
      cd node_modules/.bun/node-pty@*/node_modules/node-pty
      node-gyp rebuild
      node scripts/post-install.js
      cd -

      runHook postConfigure
    '';

    buildPhase = ''
      runHook preBuild

      for app in web server desktop; do
        bun run --cwd apps/"$app" build
      done

      runHook postBuild
    '';

    installPhase = ''
      runHook preInstall

      mkdir -p "$out"/libexec/t3code/apps/desktop "$out"/libexec/t3code/apps/server
      cp -r --no-preserve=mode node_modules "$out"/libexec/t3code
      cp -r --no-preserve=mode apps/server/{node_modules,dist} "$out"/libexec/t3code/apps/server
      cp -r --no-preserve=mode apps/desktop/{node_modules,dist-electron} "$out"/libexec/t3code/apps/desktop

      mkdir -p "$out"/libexec/t3code/apps/desktop/prod-resources
      install -m444 assets/prod/black-universal-1024.png \
        "$out"/libexec/t3code/apps/desktop/prod-resources/icon.png

      find "$out"/libexec/t3code -xtype l -delete

      makeWrapper ${lib.getExe nodejs} "$out"/bin/t3code \
        --add-flags "$out"/libexec/t3code/apps/server/dist/index.mjs

      makeWrapper ${lib.getExe electron} "$out"/bin/t3code-desktop \
        --add-flags "$out"/libexec/t3code/apps/desktop/dist-electron/main.js \
        --inherit-argv0

      mkdir -p \
        "$out"/share/icons/hicolor/1024x1024/apps \
        "$out"/share/icons/hicolor/scalable/apps
      install -m444 assets/prod/black-universal-1024.png \
        "$out"/share/icons/hicolor/1024x1024/apps/t3code.png
      install -m444 assets/prod/logo.svg \
        "$out"/share/icons/hicolor/scalable/apps/t3code.svg

      runHook postInstall
    '';

    postInstall = lib.optionalString (stdenv.buildPlatform.canExecute stdenv.hostPlatform) ''
      installShellCompletion --cmd t3code \
        --bash <("$out"/bin/t3code --completions bash) \
        --fish <("$out"/bin/t3code --completions fish) \
        --zsh <("$out"/bin/t3code --completions zsh)
    '';

    desktopItems = [
      (makeDesktopItem {
        name = "t3code";
        desktopName = "T3 Code";
        comment = finalAttrs.meta.description;
        exec = "t3code-desktop %U";
        terminal = false;
        icon = "t3code";
        startupWMClass = "T3 Code";
        categories = [ "Development" ];
      })
    ];

    passthru = {
      inherit nodeModules;
      updateScript = nix-update-script {
        extraArgs = [
          "--subpackage"
          "nodeModules"
        ];
      };
    };

    meta = {
      description = "Minimal web GUI for coding agents";
      homepage = "https://t3.codes";
      inherit (nodejs.meta) platforms;
      license = lib.licenses.mit;
      maintainers = with lib.maintainers; [
        imalison
        qweered
      ];
      mainProgram = "t3code";
    };
  }
)
