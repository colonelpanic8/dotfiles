let
  keys = import ./keys.nix;
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
      # Not managed here: its server listens on its Tailscale address and keeps
      # its own environment ID.
      msi-elliott = {
        url = "http://msi-elliott.tail40f15e.ts.net:3773/";
        environmentId = "60c44129-b35f-42bf-bb1f-346511391eee";
      };
    };
  };
  kat = {
    label = "Kat";
    hosts.kats-macbook-neo.url = "https://kats-macbook-neo.taileb3aad.ts.net/";
  };
}
