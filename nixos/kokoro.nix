{
  config,
  lib,
  makeEnable,
  ...
}:
makeEnable config "myModules.kokoro" false {
  # OpenAI-compatible TTS server (POST /v1/audio/speech) backed by Kokoro-82M.
  virtualisation.oci-containers.backend = "podman";
  virtualisation.oci-containers.containers.kokoro = {
    image = "ghcr.io/remsky/kokoro-fastapi-cpu:latest";
    autoStart = true;
    ports = ["127.0.0.1:11116:8880"];
  };

  systemd.services.podman-kokoro = {
    wants = ["network-online.target"];
    serviceConfig = {
      # oci-containers also sets Restart; use mkForce to avoid an option merge
      # conflict when both are present.
      Restart = lib.mkForce "always";
      RestartSec = "10s";
    };
  };
}
