# Ivan's Google sign-in for T3 Code's Antigravity provider, which runs agy
# with file-backed credentials in a separate profile per provider instance.
{
  config,
  lib,
  ...
}: let
  instanceId = "antigravity";
  dataDirectory = config.services.t3code.dataDirectory or "${config.home.homeDirectory}/.t3";
  profile = "${dataDirectory}/userdata/providers/antigravity/${builtins.hashString "sha256" instanceId}";
in
  lib.mkIf (config.home.username == "imalison") {
    age.secrets.t3code-antigravity-token = {
      file = ../../nixos/secrets/t3code-antigravity-token.age;
      path = "${profile}/antigravity-acp/antigravity-oauth-token";
    };
  }
