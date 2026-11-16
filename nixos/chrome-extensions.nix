# Force-installs self-hosted Chrome extensions through machine-wide policy.
#
# Chrome reads one value per policy across the files in policies/managed, so
# ExtensionSettings and ExtensionInstallForcelist must come from this single
# file; modules register extensions here instead of writing their own policy.
# Policy reaches every profile of every user on the machine.
{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.myModules.chromeExtensions;

  packCrx = name: ext:
    pkgs.runCommand "chrome-extension-${name}-${ext.version}.crx" {
      nativeBuildInputs = [pkgs.google-chrome pkgs.openssl];
    } ''
      id="$(openssl pkey -in ${ext.key} -pubout -outform DER | sha256sum | head -c32 | tr 0-9a-f a-p)"
      if [ "$id" != "${ext.id}" ]; then
        echo "${name}: key yields extension id $id, expected ${ext.id}" >&2
        exit 1
      fi
      export HOME="$TMPDIR/home"
      mkdir -p "$HOME"
      cp -R --no-preserve=mode ${ext.src} extension
      google-chrome-stable --pack-extension="$PWD/extension" --pack-extension-key=${ext.key}
      cp extension.crx "$out"
    '';

  extensionModule = {
    name,
    config,
    ...
  }: {
    options = {
      id = lib.mkOption {
        type = lib.types.strMatching "[a-p]{32}";
        description = "Extension ID; must match the signing key.";
      };
      src = lib.mkOption {
        type = lib.types.nullOr lib.types.path;
        default = null;
        description = "Unpacked extension directory, packed with `key`.";
      };
      key = lib.mkOption {
        type = lib.types.nullOr lib.types.path;
        default = null;
        description = "PKCS#8 private key that fixes the extension ID.";
      };
      version = lib.mkOption {
        type = lib.types.str;
        default = (lib.importJSON "${config.src}/manifest.json").version;
      };
      crx = lib.mkOption {
        type = lib.types.path;
        default = packCrx name config;
        description = "Packed extension; built from `src` and `key` unless set.";
      };
    };
  };

  updateManifest = name: ext:
    pkgs.writeText "chrome-extension-${name}-updates.xml" ''
      <?xml version="1.0" encoding="UTF-8"?>
      <gupdate xmlns="http://www.google.com/update2/response" protocol="2.0">
        <app appid="${ext.id}">
          <updatecheck codebase="file://${ext.crx}" version="${ext.version}" />
        </app>
      </gupdate>
    '';

  updateUrls = lib.mapAttrs (name: ext: "file://${updateManifest name ext}") cfg.forceInstalled;

  policy = {
    ExtensionInstallForcelist =
      lib.mapAttrsToList (name: ext: "${ext.id};${updateUrls.${name}}") cfg.forceInstalled;
    ExtensionSettings = lib.mapAttrs' (name: ext:
      lib.nameValuePair ext.id {
        installation_mode = "force_installed";
        override_update_url = true;
        update_url = updateUrls.${name};
      })
    cfg.forceInstalled;
  };
in {
  options.myModules.chromeExtensions.forceInstalled = lib.mkOption {
    type = lib.types.attrsOf (lib.types.submodule extensionModule);
    default = {};
    description = "Extensions Chrome installs from the Nix store on this machine.";
  };

  config = lib.mkIf (cfg.forceInstalled != {}) {
    environment.etc."opt/chrome/policies/managed/nix-extensions.json".text = builtins.toJSON policy;
  };
}
