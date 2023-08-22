{ machineNames, ... }:
{
  nix.settings.substituters = map (machineName: "ssh://${machineName}.local") machineNames;
}

