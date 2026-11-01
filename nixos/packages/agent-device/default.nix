{
  lib,
  stdenvNoCC,
  fetchurl,
  nodejs_24,
}:
stdenvNoCC.mkDerivation (finalAttrs: {
  pname = "agent-device";
  # Keep in step with T3 Code's apps/server/src/device/deviceToolVersions.json:
  # Simlock workers must run the version the T3 server pins.
  version = "0.21.31";

  src = fetchurl {
    url = "https://registry.npmjs.org/agent-device/-/agent-device-${finalAttrs.version}.tgz";
    hash = "sha512-541X/tXHqQS3Xg5PutPSek1nQFLgEJEXFdLPiZ9jTvSb+SMHr/Z4kzVCnBkGza59R+EHO1atLWx0ZAW5vK84IQ==";
  };

  buildInputs = [nodejs_24];

  installPhase = ''
    runHook preInstall
    mkdir -p $out/lib/agent-device $out/bin
    cp -r . $out/lib/agent-device/
    # A symlink, not a wrapper: Simlock finds the package by resolving it.
    ln -s $out/lib/agent-device/bin/agent-device.mjs $out/bin/agent-device
    runHook postInstall
  '';

  meta = {
    description = "Device automation CLI for coding agents";
    homepage = "https://github.com/callstackincubator/agent-device";
    license = lib.licenses.mit;
    mainProgram = "agent-device";
  };
})
