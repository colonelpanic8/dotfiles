{ makeEnable, config, ... }:
makeEnable config "myModules.laptop" true {
  services.logind = {
    lidSwitchExternalPower = "ignore";
  };
}
