{ pkgs, config, lib, makeEnable, realUsers, ... }:
makeEnable config "myModules.home-assistant" false {
  boot.kernel.sysctl = {
    # For all interfaces (e.g. if you want to accept RA on all):
    "net.ipv6.conf.all.accept_ra" = lib.mkForce "1";
    "net.ipv6.conf.all.accept_ra_rt_info_max_plen" = lib.mkForce "64";
    "net.ipv6.conf.default.accept_ra" = lib.mkForce "1";
    "net.ipv6.conf.default.accept_ra_rt_info_max_plen" = lib.mkForce "64";
    "net.ipv6.conf.wlo1.accept_ra" = lib.mkForce "1";
    "net.ipv6.conf.wlo1.accept_ra_rt_info_max_plen" = lib.mkForce "64";

    # Ensure forwarding is off on all interfaces unless needed
    "net.ipv6.conf.all.forwarding" = lib.mkForce "0";
  };

  # services.matter-server = {
  #   enable = true;
  #   logLevel = "debug";
  #   extraArgs = let cert-dir = pkgs.fetchFromGitHub {
  #     repo = "connectedhomeip";
  #     owner = "project-chip";
  #     rev = "6e8676be6142bb541fa68048c77f2fc56a21c7b1";
  #     hash = "sha256-QwPKn2R4mflTKMyr1k4xF04t0PJIlzNCOdXEiQwX5wk=";
  #   }; in
  #   [
  #     # "--bluetooth-adapter=0"
  #     "--paa-root-cert-dir=${cert-dir}/credentials/production/paa-root-certs"
  #     "--enable-test-net-dcl"
  #     "--ota-provider-dir=/var/lib/matter-server/ota-provider"
  #   ];
  # };

  # age.secrets.google-service-account = {
  #   file = ../secrets/google-assistant-integration-service-key.age;
  #   owner = "hass";
  # };

  services.zwave-js = {
    enable = true;
    serialPort = "/dev/serial/by-id/usb-Silicon_Labs_Zooz_ZST10_700_Z-Wave_Stick_fec41d5809caec11843b63a341be1031-if00-port0";
  };

  services.zwave-js-ui = {
    enable = true;
  };

  services.home-assistant = {
    enable = true;
    extraComponents = [
      "anthropic"
      "cast"
      "dlna_dmr"
      "esphome"
      "google_assistant"
      "google_translate"
      "homeassistant_hardware"
      "homeassistant_sky_connect"
      "homekit_controller"
      "hue"
      "ibeacon"
      "isal"
      "kef"
      "kegtron"
      "matter"
      "met"
      "opensky"
      "otbr"
      "piper"
      "radio_browser"
      "roomba"
      "spotify"
      "samsungtv"
      "thread"
      "wake_word"
      "webostv"
      "whisper"
      "wyoming"
      "yale"
      "zwave_js"
    ];
    extraPackages = python3Packages: with python3Packages; [
      numpy
      python-matter-server
      universal-silabs-flasher
    ];
    config = {
      http = {
        use_x_forwarded_for = true;
        trusted_proxies = ["0.0.0.0" "127.0.0.1" "::1" "192.168.50.1"];
      };
      # google_assistant = {
      #   project_id = "canyon-run-b104-home-assistant";
      #   service_account = "!include ${config.age.secrets.google-service-account.path}";
      #   report_state = true;
      #   exposed_domains = ["switch" "light"];
      # };
      default_config = {};
    };
  };
}
