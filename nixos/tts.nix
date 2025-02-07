{ config, makeEnable, ... }:
makeEnable config "myModules.tts" false {
  services.tts.servers.coqui = {
    enable = true;
    useCuda = false;
    port = 11115;
  };
}
