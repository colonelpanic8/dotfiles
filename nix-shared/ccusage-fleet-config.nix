{localHost}: let
  fleetHosts = [
    "ryzen-shine"
    "strixi-minaj"
    "jay-lenovo"
    "mac-demarco-mini"
    "railbird-sf"
  ];
in
  builtins.toJSON {
    "$schema" = "https://raw.githubusercontent.com/Open330/ccusage-fleet/v0.3.0/ccusage-fleet.schema.json";
    hosts = map (name:
      if name == localHost
      then {
        inherit name;
        type = "local";
      }
      else {
        inherit name;
        type = "ssh";
        target = name;
      })
    fleetHosts;
    timezone = "America/Los_Angeles";
    groupBy = "device";
    graph = false;
    graphMetric = "tokens";
    concurrency = 5;
    timeoutMs = 120000;
  }
  + "\n"
