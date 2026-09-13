{
  config,
  inputs,
  makeEnable,
  ...
}:
makeEnable config "myModules.googleMessages" true {
  imports = [inputs.google-messages-bridge.nixosModules.default];

  # API token for the desktop client, encrypted to every host (keys.agenixKeys)
  # so the client unlocks without typing anything on any machine.
  # The bridge server itself still runs on ryzen-shine and reads the same
  # token from `pass`; see the bridge repo docs.
  age.secrets.google-messages-bridge-api-token = {
    file = ./secrets/google-messages-bridge-api-token.age;
    owner = "imalison";
    group = "users";
    mode = "0400";
  };

  services.google-messages-multidevice-bridge.client = {
    enable = true;
    bridgeUrl = "https://ryzen-shine.taileb3aad.ts.net:8443";
    apiTokenFile = config.age.secrets.google-messages-bridge-api-token.path;
  };
}
