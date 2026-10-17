# T3 Code fleet members, each reached through Tailscale Serve on its MagicDNS
# name. Drives the desktop's managed connections and the phone enrollment
# bundle (gen-gh-pages/t3code/mint-enrollment.sh), which carries a credential
# for each of these hosts.
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
