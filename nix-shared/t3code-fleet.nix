# T3 Code fleet members, each reached through Tailscale Serve on its MagicDNS
# name. Shared by the desktop's managed connections and the servers' fleet
# manifest, which phones and unmanaged clients use to join the whole fleet.
let
  hosts = [
    "jay-lenovo"
    "jimi-hendnix"
    "mac-demarco-mini"
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
