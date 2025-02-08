{ pkgs, config, makeEnable, ... }:
makeEnable config "myModules.wyoming" false {
  environment.systemPackages = with pkgs; [
    alsa-utils
  ];
  systemd.services."wyoming-satellite".path = with pkgs; [pipewire pulseaudio];
  services.wyoming = {
    satellite = {
      enable = true;
      user = "imalison";
      uri = "tcp://0.0.0.0:10700";
      microphone = {
        # command = "pw-record --channels 1 -";
        command = "pacat --record --raw --format=s16le --channels=1 --rate=16000";
      };
      extraArgs = [
        "--wake-uri=tcp://0.0.0.0:10400"
        "--wake-word-name=ok_nabu"
        "--awake-wav=${./notification-sound.wav}"
      ];
    };
    openwakeword = {
      enable = true;
      preloadModels = ["alexa" "ok_nabu" "hey_rhasspy"];
      uri = "tcp://0.0.0.0:10400";
    };

    faster-whisper.servers."${config.networking.hostName}-whisper" = {
      enable = true;
      uri = "tcp://0.0.0.0:10300";
      device = "auto";
      language = "en";
      model = "turbo";
    };

    piper.servers."${config.networking.hostName}-piper" = {
      enable = true;
      uri = "tcp://0.0.0.0:10200";
      voice = "en-us-ryan-medium";
    };
  };
}
