{
  lib,
  stdenv,
  fetchFromGitHub,
  fetchPnpmDeps,
  pnpm_12,
  pnpmConfigHook,
  nodejs_24,
  makeBinaryWrapper,
}:
stdenv.mkDerivation (finalAttrs: {
  pname = "simlock";
  version = "0.4.2-unstable-2026-10-31";

  src = fetchFromGitHub {
    owner = "callstackincubator";
    repo = "simlock";
    rev = "944d2549b4b18577080dcd5d41ed5dfdda76283f";
    hash = "sha256-4sO3JId2CpbTYp+3AK8E+JGNA4ZlHDcKsXw0JtNnZEY=";
  };

  pnpmDeps = fetchPnpmDeps {
    inherit (finalAttrs) pname version src;
    pnpm = pnpm_12;
    fetcherVersion = 5;
    hash = "sha256-oMmLIrZigKDMNpTQMOyBOC7Cy5Cd9GlTc39IjCadNzc=";
  };

  nativeBuildInputs = [
    nodejs_24
    pnpm_12
    pnpmConfigHook
    makeBinaryWrapper
  ];

  # `pnpm build` goes through turbo, a dynamically linked binary that cannot
  # run in the sandbox; run its three steps directly.
  buildPhase = ''
    runHook preBuild
    node_modules/.bin/tsc -p tsconfig.build.json
    node_modules/.bin/vite build ui
    node scripts/build-capture.mjs
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    rm -rf node_modules
    CI=true pnpm install --prod --offline --frozen-lockfile --ignore-scripts
    mkdir -p $out/lib/simlock $out/bin
    cp -r dist node_modules package.json $out/lib/simlock/
    makeWrapper ${lib.getExe nodejs_24} $out/bin/simlock \
      --add-flags $out/lib/simlock/dist/cli/main.js
    makeWrapper ${lib.getExe nodejs_24} $out/bin/simlock-daemon \
      --add-flags $out/lib/simlock/dist/daemon/main.js
    runHook postInstall
  '';

  meta = {
    description = "Control plane for iOS simulators and Android emulators";
    homepage = "https://github.com/callstackincubator/simlock";
    license = lib.licenses.mit;
    mainProgram = "simlock";
    platforms = lib.platforms.linux;
  };
})
