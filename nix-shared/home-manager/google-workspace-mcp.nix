{
  config,
  lib,
  options,
  pkgs,
  ...
}: let
  cfg = config.myModules.googleWorkspaceMcp;
  host = "jimi-hendnix";
  endpoint = "http://127.0.0.1:19739/mcp";
  address = "${host}.taileb3aad.ts.net";
  keys = import ../keys.nix;
  knownHosts = pkgs.writeText "google-workspace-known-hosts" "${address} ${keys.jimiHendnixHostKey}\n";
  uvx = lib.getExe' pkgs.uv "uvx";
  proxy = "${uvx} --from mcp-proxy==0.15.0 mcp-proxy";
  command = pkgs.writeShellScriptBin "google-workspace-mcp" (
    if cfg.hostServer
    then ''
      set -eu
      bearer="$XDG_RUNTIME_DIR/google-workspace-mcp/bearer"
      if [ ! -s "$bearer" ]; then
        echo "Google Workspace MCP service is unavailable on ${host}" >&2
        exit 1
      fi
      API_ACCESS_TOKEN="$(cat "$bearer")"
      export API_ACCESS_TOKEN
      exec ${proxy} --transport streamablehttp ${endpoint}
    ''
    else ''
      exec ${lib.getExe' pkgs.openssh "ssh"} -a -T -o BatchMode=yes -o ConnectTimeout=8 \
        -o StrictHostKeyChecking=yes -o UserKnownHostsFile=${knownHosts} \
        imalison@${address} /etc/profiles/per-user/imalison/bin/google-workspace-mcp
    ''
  );
  server = {
    command = lib.getExe command;
  };
  statePath = "${config.xdg.stateHome}/google-workspace-mcp";
  clientPath = "${statePath}/oauth-client.age";
  antigravityProfile = let
    dataDir = config.services.t3code.dataDirectory;
    instance = builtins.hashString "sha256" "antigravity";
  in "${dataDir}/userdata/providers/antigravity/${instance}";
  credentialName = "ivanmalison@gmail.com.json";
  encryptedGrant = "${statePath}/grant.age";
  importClient = pkgs.writeShellScriptBin "google-workspace-mcp-import-client" ''
    set -euo pipefail
    umask 077
    mkdir -p ${lib.escapeShellArg statePath}
    temp=$(mktemp ${lib.escapeShellArg "${clientPath}.XXXXXX"})
    trap 'rm -f "$temp"' EXIT
    ${lib.getExe pkgs.jq} -e 'select(.installed | all(.client_id, .client_secret; type == "string" and length > 0))' \
      | ${lib.getExe pkgs.age} -R "$HOME/.ssh/id_ed25519.pub" -o "$temp"
    mv -f "$temp" ${lib.escapeShellArg clientPath}
  '';
  saveGrant = pkgs.writeShellScriptBin "google-workspace-mcp-save-grant" ''
    set -eu
    umask 077
    grant="$XDG_RUNTIME_DIR/google-workspace-mcp/credentials/${credentialName}"
    ${lib.getExe pkgs.jq} -e '.refresh_token | type == "string" and length > 0' "$grant" >/dev/null
    mkdir -p ${lib.escapeShellArg (builtins.dirOf encryptedGrant)}
    temp=$(mktemp ${lib.escapeShellArg "${encryptedGrant}.XXXXXX"})
    trap 'rm -f "$temp"' EXIT
    ${lib.getExe pkgs.age} -R "$HOME/.ssh/id_ed25519.pub" -o "$temp" "$grant"
    mv -f "$temp" ${lib.escapeShellArg encryptedGrant}
  '';
  startServer = pkgs.writeShellScript "start-google-workspace-mcp" ''
    set -eu
    umask 077
    oauth_client="$(${lib.getExe pkgs.age} -d -i "$HOME/.ssh/id_ed25519" ${lib.escapeShellArg clientPath})"
    GOOGLE_OAUTH_CLIENT_ID="$(${lib.getExe pkgs.jq} -er '(.installed // .web).client_id' <<< "$oauth_client")"
    GOOGLE_OAUTH_CLIENT_SECRET="$(${lib.getExe pkgs.jq} -er '(.installed // .web).client_secret' <<< "$oauth_client")"
    export GOOGLE_OAUTH_CLIENT_ID GOOGLE_OAUTH_CLIENT_SECRET
    export WORKSPACE_MCP_CREDENTIALS_DIR="$RUNTIME_DIRECTORY/credentials"
    mkdir -p "$WORKSPACE_MCP_CREDENTIALS_DIR"
    grant="$WORKSPACE_MCP_CREDENTIALS_DIR/${credentialName}"
    if [ ! -s "$grant" ] && [ -s ${lib.escapeShellArg encryptedGrant} ]; then
      ${lib.getExe pkgs.age} -d -i "$HOME/.ssh/id_ed25519" ${lib.escapeShellArg encryptedGrant} > "$grant.tmp"
      mv -f "$grant.tmp" "$grant"
    fi
    if [ ! -s "$RUNTIME_DIRECTORY/bearer" ]; then
      ${lib.getExe pkgs.openssl} rand -hex 32 > "$RUNTIME_DIRECTORY/bearer"
    fi
    MCP_PROXY_AUTH_TOKEN="$(cat "$RUNTIME_DIRECTORY/bearer")"
    export MCP_PROXY_AUTH_TOKEN
    exec ${proxy} --host 127.0.0.1 --port 19739 --initialization-timeout 45 --pass-environment -- \
      ${uvx} --from workspace-mcp==2.7.0 workspace-mcp --single-user \
      --permissions gmail:full calendar:full drive:full
  '';
in {
  options.myModules.googleWorkspaceMcp.hostServer = lib.mkEnableOption "the private Google Workspace MCP server on ${host}";

  config = {
    home.packages = [command] ++ lib.optionals cfg.hostServer [importClient saveGrant];
    myModules.codexGeneratedSkills.googleWorkspaceMcpCommand = server.command;

    home.activation.registerGoogleWorkspaceMcp = lib.hm.dag.entryAfter ["writeBoundary" "registerManagedMcpServers"] ''
      register_workspace_mcp() {
        local path="$1" definition="$2" temp
        mkdir -p "$(dirname "$path")"
        temp="$(mktemp "$path.XXXXXX")"
        if [ -s "$path" ]; then
          if ! ${lib.getExe pkgs.jq} -e --argjson server "$definition" \
            '.mcpServers["google-workspace"] = $server' "$path" > "$temp"; then
            rm -f "$temp"
            echo "Could not register Google Workspace MCP in $path" >&2
            return 0
          fi
        else
          ${lib.getExe pkgs.jq} -n --argjson server "$definition" \
            '{mcpServers: {"google-workspace": $server}}' > "$temp"
        fi
        mv -f "$temp" "$path"
      }

      register_workspace_mcp "$HOME/.claude.json" ${lib.escapeShellArg (builtins.toJSON (server // {type = "stdio";}))}
      register_workspace_mcp "$HOME/.gemini/settings.json" ${lib.escapeShellArg (builtins.toJSON server)}
      register_workspace_mcp "$HOME/.gemini/config/mcp_config.json" ${lib.escapeShellArg (builtins.toJSON server)}
      ${lib.optionalString (options.services ? t3code && config.services.t3code.enable) ''
        register_workspace_mcp ${lib.escapeShellArg "${antigravityProfile}/config/mcp_config.json"} ${lib.escapeShellArg (builtins.toJSON server)}
      ''}
    '';

    systemd.user.services.google-workspace-mcp = lib.mkIf cfg.hostServer {
      Unit = {
        Description = "Private Google Workspace MCP service";
        ConditionPathExists = clientPath;
        After = ["agenix.service"];
      };
      Service = {
        ExecStart = "${startServer}";
        RuntimeDirectory = "google-workspace-mcp";
        RuntimeDirectoryMode = "0700";
        RuntimeDirectoryPreserve = "restart";
        UMask = "0077";
        Restart = "on-failure";
        RestartSec = 5;
        Environment = [
          "USER_GOOGLE_EMAIL=ivanmalison@gmail.com"
          "WORKSPACE_MCP_BASE_URI=http://127.0.0.1"
          "WORKSPACE_MCP_HOST=127.0.0.1"
          "WORKSPACE_MCP_PORT=19740"
          "GOOGLE_OAUTH_REDIRECT_URI=http://127.0.0.1:19740/oauth2callback"
          "OAUTHLIB_INSECURE_TRANSPORT=1"
        ];
      };
      Install.WantedBy = ["default.target"];
    };
  };
}
