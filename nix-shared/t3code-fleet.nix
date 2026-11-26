# T3 Code servers sharing Ivan's managed fleet credential, reached through
# Tailscale Serve on their MagicDNS names. The mobile enrollment manifest
# also includes family servers with separate credentials.
let
  hosts = [
    "jay-lenovo"
    "jimi-hendnix"
    "mac-demarco-mini"
    "railbird-sf"
    "ryzen-shine"
    "strixi-minaj"
  ];
  magicDnsSuffix = "taileb3aad.ts.net";
in
  map (host: let
    authority = "${host}.${magicDnsSuffix}";
  in {
    environmentId = "fleet:${host}";
    label = host;
    httpBaseUrl = "https://${authority}/";
    wsBaseUrl = "wss://${authority}/";
  })
  hosts
