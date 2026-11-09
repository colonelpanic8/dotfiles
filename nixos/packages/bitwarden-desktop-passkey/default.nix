{
  bitwarden-desktop,
  fetchFromGitHub,
  lib,
  pinentry-qt,
  python3,
  writeShellApplication,
  clientsSrc ?
    fetchFromGitHub {
      owner = "colonelpanic8";
      repo = "clients";
      rev = "9791e71135ebc046b29195c4e6194008ed07589a";
      hash = "sha256-9aYadzjwnvh9dHVE+p6Sq+wyxB5LCK6iqTvw694nShk=";
    },
}: let
  python = python3.withPackages (p: [p.fido2]);
  helper = writeShellApplication {
    name = "bitwarden-passkey-helper";
    text = ''
      exec ${python}/bin/python3 -I ${./passkey-helper.py} --pinentry ${lib.getExe' pinentry-qt "pinentry"} "$@"
    '';
  };
in
  bitwarden-desktop.overrideAttrs (old: {
    version = "${old.version}-passkey";
    src = clientsSrc;
    postInstall =
      (old.postInstall or "")
      + ''
        wrapProgram "$out/bin/bitwarden" --set BITWARDEN_PASSKEY_HELPER ${lib.getExe helper}
      '';
    passthru = old.passthru // {inherit helper;};
    meta =
      old.meta
      // {
        changelog = "https://github.com/colonelpanic8/clients/commits/desktop-passkey-login";
      };
  })
