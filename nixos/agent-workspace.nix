{
  pkgs,
  config,
  lib,
  makeEnable,
  ...
}: let
  # agent-sh/agent-workspace-linux: an isolated, hidden X11 desktop (Xvfb +
  # openbox) that agents drive over MCP stdio, instead of stealing focus on the
  # real desktop. The `viewer` subcommand opens a floating window mirroring the
  # workspace for watching / briefly taking over.
  awl = pkgs.agent-workspace-linux;
  awlBin = lib.getExe awl;

  # Claude Code reads MCP server definitions from the top-level `mcpServers`
  # key of ~/.claude.json; merged idempotently below (same pattern and
  # rationale as claude-mcp.nix).
  claudeServerJson = builtins.toJSON {
    type = "stdio";
    command = awlBin;
    args = ["mcp"];
  };

  desktop = config.myModules.desktop.enable;
in
  makeEnable config "myModules.agentWorkspace" true {
    environment.systemPackages = lib.mkIf desktop [awl];

    home-manager.users.imalison = lib.mkIf desktop ({lib, ...}: {
      # Codex: shipped through the Nix-managed fragment that
      # codex-generated-skills.nix merges into ~/.codex/config.toml.
      myModules.codexGeneratedSkills.extraManagedConfig = ''
        [mcp_servers.agent-workspace-linux]
        command = "${awlBin}"
        args = ["mcp"]
      '';

      home.activation.registerAgentWorkspaceMcp = lib.hm.dag.entryAfter ["writeBoundary"] ''
        config="$HOME/.claude.json"
        server=${lib.escapeShellArg claudeServerJson}
        if [ -f "$config" ]; then
          tmp="$(mktemp)"
          if ${pkgs.jq}/bin/jq --argjson srv "$server" \
            '.mcpServers = ((.mcpServers // {}) + {"agent-workspace-linux": $srv})' \
            "$config" > "$tmp"; then
            mv -f "$tmp" "$config"
          else
            rm -f "$tmp"
            echo "agent-workspace: failed to update $config; left unchanged" >&2
          fi
        else
          ${pkgs.jq}/bin/jq -n --argjson srv "$server" \
            '{mcpServers: {"agent-workspace-linux": $srv}}' > "$config"
          chmod 600 "$config"
        fi
      '';
    });
  }
