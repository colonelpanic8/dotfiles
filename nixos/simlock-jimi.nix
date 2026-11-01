{
  config,
  lib,
  pkgs,
  ...
}: let
  gatewayPort = 4700;
  tailscaleServePort = 4700;
  gatewayOrigin = "https://${config.networking.hostName}.taileb3aad.ts.net:${toString tailscaleServePort}";

  android = pkgs.androidenv.composeAndroidPackages {
    includeEmulator = true;
  };
  sdkSource = "${android.androidsdk}/libexec/android-sdk";
  jdk = pkgs.jdk21_headless;

  simlock = pkgs.symlinkJoin {
    name = "simlock-${pkgs.simlock.version}";
    paths = [pkgs.simlock];
    nativeBuildInputs = [pkgs.makeWrapper];
    postBuild = ''
      for program in simlock simlock-daemon; do
        wrapProgram $out/bin/$program \
          --run 'export ANDROID_HOME="''${ANDROID_HOME:-$HOME/.local/share/simlock/android-sdk}"' \
          --set JAVA_HOME ${jdk} \
          --prefix PATH : ${lib.makeBinPath [jdk pkgs.agent-device]}
      done
    '';
  };
  cli = "${simlock}/bin/simlock";

  # nixpkgs' sdkmanager pins --sdk_root to the store; these resolve the SDK
  # from wherever cmdline-tools sits, as the upstream launchers do.
  sdkmanager = pkgs.writeShellScript "sdkmanager" ''
    tools="$(cd "$(dirname "$0")/.." && pwd -P)"
    exec ${jdk}/bin/java -Dcom.android.sdkmanager.toolsdir="$tools" $JAVA_OPTS $SDKMANAGER_OPTS \
      -classpath "$tools/lib/sdklib/tools.sdklib.jar:$tools/lib/avdmanager-classpath.jar" \
      com.android.sdklib.tool.sdkmanager.SdkManagerCli --sdk_root="$(dirname "$(dirname "$tools")")" "$@"
  '';
  avdmanager = pkgs.writeShellScript "avdmanager" ''
    JAVA_HOME=${jdk} exec "$(dirname "$0")/.avdmanager-wrapped" "$@"
  '';

  # cmdline-tools is a real copy so system images and AVD metadata land in the
  # writable SDK. The emulator and platform-tools come patched from nixpkgs.
  prepareSdk = pkgs.writeShellApplication {
    name = "simlock-prepare-sdk";
    runtimeInputs = [pkgs.coreutils];
    text = ''
      sdk="$HOME/.local/share/simlock/android-sdk"
      mkdir -p "$sdk/cmdline-tools"
      ln -sfn ${sdkSource}/emulator "$sdk/emulator"
      ln -sfn ${sdkSource}/platform-tools "$sdk/platform-tools"
      # Simlock's acceptAndroidLicenses does not answer sdkmanager's prompt.
      mkdir -p "$sdk/licenses"
      cp --update=none ${sdkSource}/licenses/* "$sdk/licenses/"
      chmod u+w "$sdk"/licenses/*
      tools="$(readlink -f ${sdkSource}/cmdline-tools/*)"
      stamp="$tools ${sdkmanager} ${avdmanager}"
      if [ "$(cat "$sdk/cmdline-tools/.source" 2>/dev/null)" != "$stamp" ]; then
        rm -rf "$sdk/cmdline-tools/latest"
        cp -rL "$tools" "$sdk/cmdline-tools/latest"
        chmod -R u+w "$sdk/cmdline-tools/latest"
        install -m755 ${sdkmanager} "$sdk/cmdline-tools/latest/bin/sdkmanager"
        install -m755 ${avdmanager} "$sdk/cmdline-tools/latest/bin/avdmanager"
        echo "$stamp" > "$sdk/cmdline-tools/.source"
      fi
    '';
  };

  configureGateway = pkgs.writeShellApplication {
    name = "simlock-configure-gateway";
    text = ''
      set_key() { ${cli} --gateway config set "$1" "$2" >/dev/null; }
      set_key http.host 127.0.0.1
      set_key http.port ${toString gatewayPort}
      set_key lease.maxPerRequester 2
      set_key capabilities.stream.enabled true
      set_key capabilities.input.enabled true
      set_key capabilities.agent-device.enabled true
    '';
  };

  # Join tokens and agent tokens are minted by the gateway, so they are
  # created here on first start rather than kept in the repository.
  configureWorker = pkgs.writeShellApplication {
    name = "simlock-configure-worker";
    runtimeInputs = [pkgs.coreutils pkgs.curl pkgs.jq];
    text = ''
      set_key() { ${cli} --local config set "$1" "$2" >/dev/null; }
      set_key android.emulator.headless true
      set_key downloads.acceptAndroidLicenses true
      set_key warmPool.enabled false
      set_key capabilities.stream.enabled true
      set_key capabilities.input.enabled true
      set_key capabilities.agent-device.enabled true
      set_key capabilities.agent-device.command ${pkgs.agent-device}/bin/agent-device

      # Without a running gateway, `simlock --gateway` would start a stray one.
      healthy=""
      for _ in $(seq 60); do
        if curl -sf http://127.0.0.1:${toString gatewayPort}/v1/healthz >/dev/null; then
          healthy=1
          break
        fi
        sleep 1
      done
      [ -n "$healthy" ]

      mint() {
        ${cli} --gateway token create --role "$1" --label "$2" | jq -er .secret
      }

      config="''${SIMLOCK_HOME:-$HOME/.simlock}/config.json"
      if ! jq -e '.gateway.token' "$config" >/dev/null 2>&1; then
        token="$(mint worker ${config.networking.hostName})"
        set_key gateway "$(jq -cn --arg token "$token" \
          '{url: "ws://127.0.0.1:${toString gatewayPort}", token: $token, label: "${config.networking.hostName}"}')"
      fi

      env_file="$HOME/.config/simlock/t3-agent.env"
      if [ ! -s "$env_file" ]; then
        mkdir -p "$(dirname "$env_file")"
        token="$(mint agent t3-${config.networking.hostName})"
        (
          umask 077
          printf 'AGENT_DEVICE_DAEMON_BASE_URL=%s/v1/agent-device\nAGENT_DEVICE_DAEMON_AUTH_TOKEN=%s\n' \
            ${gatewayOrigin} "$token" > "$env_file"
        )
      fi
    '';
  };

  daemonService = description: extra:
    lib.recursiveUpdate {
      Unit.Description = description;
      Install.WantedBy = ["default.target"];
      Service = {
        Restart = "on-failure";
        RestartSec = 10;
        # Emulators are children of the worker; a restart re-adopts them.
        KillMode = "process";
        TimeoutStopSec = 300;
      };
    }
    extra;
in {
  assertions = [
    {
      assertion = config.networking.hostName == "jimi-hendnix";
      message = "simlock-jimi.nix must only be imported on jimi-hendnix";
    }
  ];

  environment.systemPackages = [simlock];

  # The host carries no token: T3 falls back to AGENT_DEVICE_DAEMON_* from
  # the env file the worker's first start writes.
  myModules.t3codeServer.settings = {
    simlockDeviceHostsEnabled = true;
    simlockDeviceHosts = [
      {
        kind = "simlock";
        id = "jimi-hendnix-simlock";
        label = "jimi-hendnix Simlock";
        gatewayUrl = gatewayOrigin;
        maxLeases = 2;
        maxLeaseMinutes = 60;
      }
    ];
  };

  home-manager.users.imalison = {
    systemd.user.services.simlock-gateway = daemonService "Simlock gateway" {
      Service = {
        ExecStartPre = lib.getExe configureGateway;
        ExecStart = "${simlock}/bin/simlock-daemon --gateway";
      };
    };

    systemd.user.services.simlock-worker = daemonService "Simlock Android worker" {
      Unit = {
        After = ["simlock-gateway.service"];
        Wants = ["simlock-gateway.service"];
      };
      Service = {
        ExecStartPre = [(lib.getExe prepareSdk) (lib.getExe configureWorker)];
        ExecStart = "${simlock}/bin/simlock-daemon";
      };
    };

    systemd.user.services.t3code-headless.Service.EnvironmentFile = "-%h/.config/simlock/t3-agent.env";
  };

  systemd.services.simlock-gateway-serve = {
    description = "Tailscale Serve mapping for the Simlock gateway";
    after = ["tailscaled.service"];
    wants = ["tailscaled.service"];
    wantedBy = ["multi-user.target"];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      TimeoutStartSec = "60s";
      ExecStartPre = "${config.services.tailscale.package}/bin/tailscale wait --timeout=45s";
      ExecStart = "${pkgs.util-linux}/bin/flock /run/tailscale-serve.lock ${config.services.tailscale.package}/bin/tailscale serve --bg --https=${toString tailscaleServePort} --set-path=/ http://127.0.0.1:${toString gatewayPort}";
      Restart = "on-failure";
      RestartSec = "5s";
    };
  };
}
