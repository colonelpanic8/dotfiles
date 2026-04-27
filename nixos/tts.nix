{ config, makeEnable, ... }:
makeEnable config "myModules.tts" false {
  services.tts.servers.coqui = {
    enable = true;
    useCuda = false;
    port = 11115;
    model = "tts_models/en/vctk/vits";
    extraArgs = [ "--speaker_idx" "p376" ];
  };

  systemd.services.tts-coqui.wants = [ "network-online.target" ];
}
