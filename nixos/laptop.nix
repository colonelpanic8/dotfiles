{ makeEnable, config, ... }:
makeEnable config "myModules.laptop" true {
  services.logind.settings.Login.HandleLidSwitchExternalPower = "ignore";
}
