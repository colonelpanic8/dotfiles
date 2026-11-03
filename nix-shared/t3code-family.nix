let
  keys = import ./keys.nix;
  alexFleet = builtins.listToAttrs (map (member: {
    name = member.label;
    value = member;
  }) (import ./t3code-fleet.alex.nix));
in {
  alex = {
    label = "Alex";
    hosts = {
      alexanders-macbook-air = {
        sshHost = "alexanders-macbook-air.tail40f15e.ts.net";
        sshUser = "alex";
        hostKey = keys.alexandersMacbookAirHostKey;
        localPort = 13774;
      };
      # Its server listens on its Tailscale address and keeps its own
      # environment ID; both come from Alex's fleet manifest.
      msi-elliott = {
        url = alexFleet.msi-elliott.httpBaseUrl;
        inherit (alexFleet.msi-elliott) environmentId;
      };
    };
  };
  kat = {
    label = "Kat";
    hosts.kats-macbook-neo.url = "https://kats-macbook-neo.taileb3aad.ts.net/";
  };
}
