{ pkgs, config, makeEnable, ... }:
makeEnable config "myModules.wyoming" false {
  environment.systemPackages = with pkgs; [
    alsa-utils
  ];
  systemd.services."wyoming-satellite".path = with pkgs; [pipewire];
  services.wyoming = {
    satellite = {
      enable = true;
      package = pkgs.wyoming-satellite;
      user = "imalison";
      microphone = {
        command = "pw-record --channels 1 -";
      };
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
