{
  config,
  lib,
  pkgs,
  ...
}: let
  # Peers that enable myModules.cache-server (keep in sync with machines/*.nix).
  # jimi-hendnix also runs one but has been off the tailnet for months; re-add it
  # here when it comes back.
  cacheHosts = [
    {
      name = "ryzen-shine";
      port = 3090;
    }
    {
      name = "strixi-minaj";
      port = 3090;
    }
  ];
  peers = builtins.filter (h: h.name != config.networking.hostName) cacheHosts;
  # Bare hostnames resolve over tailscale MagicDNS (and LAN DNS when home).
  peerUrl = h: "http://${h.name}:${toString h.port}";

  # Peers are enabled at runtime: nix.conf includes a file rendered from the
  # set of disabled peers, so toggling one needs no rebuild.
  stateDir = "/var/lib/nix-peer-caches";
  nixPeerCache = pkgs.writeShellApplication {
    name = "nix-peer-cache";
    runtimeInputs = with pkgs; [coreutils gnugrep systemd];
    text = ''
      state_dir=${stateDir}
      disabled_file="$state_dir/disabled"
      conf_file="$state_dir/nix.conf"
      declare -A urls=(${lib.concatMapStrings (h: "[${h.name}]=${peerUrl h} ") peers})

      usage() {
        echo "usage: nix-peer-cache [status | urls | render | enable|disable|toggle <peer>...]" >&2
        echo "peers: ''${!urls[*]}" >&2
        exit 2
      }

      is_disabled() { [ -f "$disabled_file" ] && grep -qxF "$1" "$disabled_file"; }

      render() {
        local enabled=() name
        for name in "''${!urls[@]}"; do
          is_disabled "$name" || enabled+=("''${urls[$name]}")
        done
        mkdir -p "$state_dir"
        printf 'extra-substituters = %s\n' "''${enabled[*]}" > "$conf_file.tmp"
        mv "$conf_file.tmp" "$conf_file"
      }

      as_root() {
        if [ "$(id -u)" -ne 0 ]; then
          exec /run/wrappers/bin/sudo "$0" "$@"
        fi
      }

      cmd="''${1:-status}"
      [ $# -gt 0 ] && shift
      case "$cmd" in
        status)
          for name in "''${!urls[@]}"; do
            if is_disabled "$name"; then state=disabled; else state=enabled; fi
            printf '%-16s %-8s %s\n' "$name" "$state" "''${urls[$name]}"
          done
          ;;
        urls)
          for name in "''${!urls[@]}"; do
            is_disabled "$name" || echo "''${urls[$name]}"
          done
          ;;
        render)
          as_root "$cmd" "$@"
          render
          ;;
        enable | disable | toggle)
          [ $# -gt 0 ] || usage
          for name in "$@"; do
            [ -n "''${urls[$name]+x}" ] || { echo "unknown peer: $name" >&2; usage; }
          done
          as_root "$cmd" "$@"
          mkdir -p "$state_dir"
          touch "$disabled_file"
          for name in "$@"; do
            action="$cmd"
            if [ "$action" = toggle ]; then
              if is_disabled "$name"; then action=enable; else action=disable; fi
            fi
            grep -vxF "$name" "$disabled_file" > "$disabled_file.tmp" || true
            [ "$action" = disable ] && echo "$name" >> "$disabled_file.tmp"
            mv "$disabled_file.tmp" "$disabled_file"
            echo "$name: ''${action}d"
          done
          render
          # KillMode=process: in-flight builds keep running under the old config.
          systemctl restart nix-daemon.service
          ;;
        *) usage ;;
      esac
    '';
  };
in {
  environment.systemPackages = [nixPeerCache];

  system.activationScripts.nix-peer-caches = "${lib.getExe nixPeerCache} render";

  nix.extraOptions = ''
    !include ${stateDir}/nix.conf
  '';

  nix.settings = {
    # Lets `just switch` pass the enabled peers explicitly.
    extra-trusted-substituters = map peerUrl peers;
    extra-trusted-public-keys = [(lib.fileContents ./secrets/cache-pub-key.pem)];
    # Skip unreachable peer caches after a few seconds instead of hanging.
    # Nix retries failed downloads with backoff, so a dead peer costs
    # download-attempts * connect-timeout once per invocation.
    connect-timeout = 3;
    download-attempts = 2;
    # If substitution fails (peer offline mid-download), build locally.
    fallback = true;
  };
}
