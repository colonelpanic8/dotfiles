{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.services.mac-camera;
  user = config.system.primaryUser;
  home = config.users.users.${user}.home;
  capture = pkgs.writeShellScriptBin "mac-camera" ''
    exec ${pkgs.python3}/bin/python3 ${./camera/capture.py} --owner ${lib.escapeShellArg user} "$@"
  '';
  relayConfig = pkgs.writeText "camera-mediamtx.json" (builtins.toJSON {
    logLevel = "warn";
    api = false;
    metrics = false;
    pprof = false;
    playback = false;
    rtsp = false;
    rtmp = true;
    rtmpAddress = "127.0.0.1:18379";
    hls = true;
    hlsAddress = "127.0.0.1:18380";
    hlsAllowOrigins = [];
    hlsVariant = "mpegts";
    hlsSegmentCount = 3;
    hlsSegmentDuration = "2s";
    hlsDirectory = "";
    webrtc = false;
    srt = false;
    moq = false;
    authInternalUsers = [
      {
        user = "any";
        ips = ["127.0.0.1"];
        permissions = [
          {
            action = "publish";
            path = "camera";
          }
          {
            action = "read";
            path = "camera";
          }
        ];
      }
    ];
    paths.camera = {
      source = "publisher";
      record = false;
    };
  });
  relay = pkgs.writeShellScript "camera-relay" ''
    set -eu
    state=${lib.escapeShellArg "${home}/Library/Application Support/TailnetCamera/relay"}
    ${pkgs.coreutils}/bin/install -d -m 700 "$state"
    ${pkgs.coreutils}/bin/install -m 600 ${relayConfig} "$state/mediamtx.yml"
    exec ${pkgs.mediamtx}/bin/mediamtx "$state/mediamtx.yml"
  '';
  service = args: {
    serviceConfig = {
      ProgramArguments = args;
      UserName = user;
      RunAtLoad = true;
      KeepAlive = true;
      ProcessType = "Background";
      StandardErrorPath = "${home}/Library/Logs/mac-camera-services.err.log";
      StandardOutPath = "${home}/Library/Logs/mac-camera-services.log";
    };
  };
  serve = pkgs.writeShellScript "camera-tailscale-serve" ''
    set -eu
    if [ -x /Applications/Tailscale.app/Contents/MacOS/Tailscale ]; then
      tailscale=/Applications/Tailscale.app/Contents/MacOS/Tailscale
    else
      tailscale=${pkgs.tailscale}/bin/tailscale
    fi
    exec ${pkgs.coreutils}/bin/timeout 30 "$tailscale" serve --bg --https=8443 http://127.0.0.1:18381
  '';
in {
  options.services.mac-camera = {
    enable = lib.mkEnableOption "private unattended camera and microphone streaming";
    viewer = lib.mkOption {
      type = lib.types.str;
      description = "Exact Tailscale user login allowed to read the feed.";
    };
  };
  config = lib.mkIf cfg.enable {
    homebrew.casks = ["obs"];
    environment.systemPackages = [capture];
    launchd.daemons.mac-camera-relay = service ["${relay}"];
    launchd.daemons.mac-camera-gateway = service [
      "${pkgs.python3}/bin/python3"
      "${./camera/proxy.py}"
      "--viewer"
      cfg.viewer
    ];
    launchd.daemons.mac-camera-serve = {
      serviceConfig = {
        ProgramArguments = ["${serve}"];
        UserName = user;
        RunAtLoad = true;
        StartInterval = 300;
        StandardErrorPath = "${home}/Library/Logs/mac-camera-serve.err.log";
        StandardOutPath = "${home}/Library/Logs/mac-camera-serve.log";
      };
    };
    home-manager.users.${user}.launchd.agents.mac-camera = {
      enable = true;
      domain = "user";
      config = {
        ProgramArguments = ["${capture}/bin/mac-camera" "run"];
        RunAtLoad = true;
        KeepAlive.SuccessfulExit = false;
        ProcessType = "Interactive";
        StandardOutPath = "${home}/Library/Logs/mac-camera.log";
        StandardErrorPath = "${home}/Library/Logs/mac-camera.err.log";
      };
    };
  };
}
