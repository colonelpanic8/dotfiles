# Tracks Antigravity ahead of nixpkgs. Versions and build IDs come from the
# same endpoints the nixpkgs update scripts read:
#   cli: https://storage.googleapis.com/antigravity-public/antigravity-cli/latest
#   acp: https://raw.githubusercontent.com/agentclientprotocol/registry/main/antigravity-acp/agent.json
#   hub: https://antigravity-hub-auto-updater-974169037036.us-central1.run.app/manifest/latest-x64-linux.yml
#   ide: https://antigravity-ide-auto-updater-974169037036.us-central1.run.app/api/update/linux-x64/stable/latest
final: prev: let
  system = prev.stdenv.hostPlatform.system;

  cli = {
    version = "1.5.5";
    buildId = "6740358537805824";
    sources = {
      x86_64-linux = {
        file = "linux-x64/cli_linux_x64.tar.gz";
        hash = "sha256-v+LCXt6t2YwpQkF8mXLuDh+qPwr9yQcR6E1fOmM7UHg=";
      };
      aarch64-linux = {
        file = "linux-arm/cli_linux_arm64.tar.gz";
        hash = "sha256-9vr/e0etLZpg68qdd0I5oc0j5TqMptgSwelYOQncplU=";
      };
    };
  };

  acp = {
    version = "1.4.5";
    sources = {
      x86_64-linux = {
        arch = "x86_64";
        hash = "sha256-kjkjoNfcgHP2bMb3hkejEH46fuK34aJN+LOSe1GIATs=";
      };
      aarch64-linux = {
        arch = "arm64";
        hash = "sha256-fWQkZjOuCU9n47vNLWeeiQSPkTRtUzkxMzbnSD9A8FY=";
      };
    };
  };

  hub = {
    version = "2.31.0";
    buildId = "6379060755038208";
    sources = {
      x86_64-linux = {
        platform = "linux-x64";
        hash = "sha256-MMzeof/wv2Q7f256pJMkDRPsHYZqz0+JwqbMOiwzNpw=";
      };
      aarch64-linux = {
        platform = "linux-arm";
        hash = "sha256-CzzdF/qsDo7h+ASUeHrB73j1QEvCFszs8OP+lZGLLaw=";
      };
    };
  };

  ide = {
    version = "2.6.0";
    buildId = "5526304964083712";
    sources = {
      x86_64-linux = {
        platform = "linux-x64";
        hash = "sha256-sGCnx2eRA7K2GXUvjuejCI3SQafSB9QxGXS1bqb5Q1Y=";
      };
      aarch64-linux = {
        platform = "linux-arm";
        hash = "sha256-SVPMtSv5CVgqFxGj3SbWQO+0X+VVoX6+/4vH4uqgUt4=";
      };
    };
  };

  bump = name: spec: mkAttrs:
    if spec.sources ? ${system}
    then prev.${name}.overrideAttrs (mkAttrs spec.sources.${system})
    else prev.${name};
in {
  antigravity-cli = bump "antigravity-cli" cli (src: {
    inherit (cli) version;
    src = prev.fetchurl {
      url = "https://storage.googleapis.com/antigravity-public/antigravity-cli/${cli.version}-${cli.buildId}/${src.file}";
      inherit (src) hash;
    };
  });

  antigravity-acp = bump "antigravity-acp" acp (src: {
    inherit (acp) version;
    src = prev.fetchurl {
      url = "https://dl.google.com/agy-extensions/releases/linux/agy-acp-server-${acp.version}-linux-${src.arch}.zip";
      inherit (src) hash;
    };
  });

  antigravity-hub = bump "antigravity-hub" hub (src: old: {
    inherit (hub) version;
    src = prev.fetchurl {
      url = "https://storage.googleapis.com/antigravity-public/antigravity-hub/${hub.version}-${hub.buildId}/${src.platform}/Antigravity.tar.gz";
      inherit (src) hash;
    };
    passthru = old.passthru // {inherit (hub) buildId;};
  });

  antigravity-ide = bump "antigravity-ide" ide (src: {
    inherit (ide) version;
    src = prev.fetchurl ({
        url = "https://edgedl.me.gvt1.com/edgedl/release2/j0qc3/antigravity/stable/${ide.version}-${ide.buildId}/${src.platform}/Antigravity%20IDE.tar.gz";
      }
      // removeAttrs src ["platform"]);
  });
}
