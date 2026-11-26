let
  inherit ((import ./t3code-family.nix).alex.hosts.malisonlaptop) environmentId label url;
in
  (import ./t3code-fleet.nix)
  ++ [
    {
      inherit environmentId label;
      httpBaseUrl = url;
      wsBaseUrl = builtins.replaceStrings ["https://"] ["wss://"] url;
      reuseEnrollment = true;
    }
  ]
