{ config, makeEnable, ... }:
makeEnable config "myModules.wyoming" false {
  services.wyoming = {
    satellite = {
      enable = true;
      user = "imalison";
    };
    faster-whisper.servers.strixi = {
      enable = true;
      uri = "tcp://0.0.0.0:10300";
      device = "auto";
      language = "en";
    };
    piper.servers.strixi = {
      enable = true;
      uri = "tcp://0.0.0.0:10200";
      voice = "en-us-ryan-medium";
    };
    openwakeword = {
      enable = true;
      preloadModels = ["alexa" "ok_nabu" "hey_rhasspy"];
    };
  };
}
