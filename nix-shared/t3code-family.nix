let
  keys = import ./keys.nix;
in {
  alex = {
    label = "Alex";
    hostName = "alexanders-macbook-air";
    sshHost = "alexanders-macbook-air.tail40f15e.ts.net";
    sshUser = "alex";
    hostKey = keys.alexandersMacbookAirHostKey;
    localPort = 13774;
  };
  kat = {
    label = "Kat";
    hostName = "kats-macbook-neo";
    sshHost = "kats-macbook-neo.taileb3aad.ts.net";
    sshUser = "kat";
    hostKey = keys.katsMacbookNeoHostKey;
    localPort = 13775;
  };
}
