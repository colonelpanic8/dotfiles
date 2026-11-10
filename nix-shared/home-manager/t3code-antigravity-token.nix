# Ivan's Google sign-in for T3 Code's Antigravity provider. T3 keeps one
# profile per provider instance, keyed by the SHA-256 of the instance id. The
# runtime rewrites the token in place with the same bytes, and agenix relinks
# it on its next run.
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
      path = "${profile}/antigravity-acp/acp_token.json";
    };
  }
