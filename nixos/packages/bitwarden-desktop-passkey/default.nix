{
  bitwarden-desktop,
  fetchFromGitHub,
  fetchNpmDeps,
  lib,
  makeWrapper,
  pinentry-qt,
  python3,
  rustPlatform,
  symlinkJoin,
  writeShellApplication,
  clientsSrc ?
    fetchFromGitHub {
      owner = "colonelpanic8";
      repo = "clients";
      rev = "9791e71135ebc046b29195c4e6194008ed07589a";
      hash = "sha256-9aYadzjwnvh9dHVE+p6Sq+wyxB5LCK6iqTvw694nShk=";
    },
}: let
  version = "2026.10.0";
  python = python3.withPackages (p: [p.fido2]);
  helper = writeShellApplication {
    name = "bitwarden-passkey-helper";
    text = ''
      exec ${python}/bin/python3 -I ${./passkey-helper.py} --pinentry ${lib.getExe' pinentry-qt "pinentry"} "$@"
    '';
  };
  app = bitwarden-desktop.overrideAttrs (final: old: {
    version = "${version}-passkey";
    src = clientsSrc;
    npmDeps = fetchNpmDeps {
      inherit (final) src;
      name = "bitwarden-desktop-${version}-npm-deps";
      fetcherVersion = 2;
      hash = "sha256-Ygom2n78W7voqar+i+l7dOjDIfrWMvR++oC8SiGOAS8=";
    };
    cargoDeps = rustPlatform.fetchCargoVendor {
      inherit (final) pname version src cargoRoot patches;
      hash = "sha256-IYDZOIt0hT+8+CTfnq09PSpETmAUsiqlj7mMamcr5DY=";
    };
    meta =
      old.meta
      // {
        changelog = "https://github.com/colonelpanic8/clients/commits/desktop-passkey-login";
      };
  });
in
  # Wrapped outside the app's own build so helper changes don't rebuild Electron.
  symlinkJoin {
    name = "bitwarden-desktop-${app.version}";
    inherit (app) version meta;
    paths = [app];
    nativeBuildInputs = [makeWrapper];
    postBuild = ''
      wrapProgram "$out/bin/bitwarden" --set BITWARDEN_PASSKEY_HELPER ${lib.getExe helper}
    '';
    passthru = {inherit app helper;};
  }
