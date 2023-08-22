{ machineNames, ... }:
{
  nix = {
    binaryCaches = map (machineName: "ssh://${machineName}.local") machineNames;
  };
}

