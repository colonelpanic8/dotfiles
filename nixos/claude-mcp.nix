{
  pkgs,
  config,
  lib,
  makeEnable,
  ...
}: let
  # The MCP-NixOS server (https://mcp-nixos.io) — gives Claude accurate NixOS
  # package/option/Home-Manager/flake search instead of hallucinated names.
  # Pinned by Nix from nixpkgs, so no runtime `uvx`/`nix run` fetch is needed.
  mcpNixosBin = "${pkgs.mcp-nixos}/bin/mcp-nixos";

  # Claude Code reads MCP server *definitions* from the top-level `mcpServers`
  # key of ~/.claude.json (the user scope). That is the only scope that applies
  # to every project without an approval prompt while remaining additive:
  #   - settings.json (our nix-managed dotfiles file) cannot define servers,
  #     only filter them (enabled/disabledMcpjsonServers).
  #   - /etc/claude-code/managed-mcp.json would take *exclusive* control and
  #     disable every other server (per-project playwright, future `mcp add`).
  # So we merge into the user config rather than owning a whole file.
  managedServers =
    {
      nixos = {
        type = "stdio";
        command = mcpNixosBin;
      };
    }
    // lib.optionalAttrs config.myModules.desktop.enable {
      "computer-use-linux" = {
        type = "stdio";
        command = "${lib.getExe pkgs.computer-use-linux}";
        args = ["mcp"];
      };
    };
  managedServersJson = builtins.toJSON managedServers;
in
  makeEnable config "myModules.claudeMcpNixos" true {
    # Also expose the pinned binary on PATH for manual `claude mcp` use / Codex.
    environment.systemPackages =
      [pkgs.mcp-nixos]
      ++ lib.optional config.myModules.desktop.enable pkgs.computer-use-linux;

    # Use a module function so `lib` here is home-manager's lib (which carries
    # `lib.hm.dag`), not the plain NixOS lib.
    home-manager.users.imalison = {lib, ...}: {
      # ~/.claude.json is mutable state owned by Claude Code, so it can't be
      # managed as a whole file. Instead idempotently merge our server into it
      # on every switch with jq, preserving all other (per-project, user-added)
      # servers and state. This module is the declarative source of truth:
      # `claude mcp remove nixos` is re-applied on the next switch.
      home.activation.registerManagedMcpServers = lib.hm.dag.entryAfter ["writeBoundary"] ''
        config="$HOME/.claude.json"
        servers=${lib.escapeShellArg managedServersJson}
        if [ -f "$config" ]; then
          tmp="$(mktemp)"
          if ${pkgs.jq}/bin/jq --argjson servers "$servers" \
            '.mcpServers = ((.mcpServers // {}) + $servers)' \
            "$config" > "$tmp"; then
            mv -f "$tmp" "$config"
          else
            rm -f "$tmp"
            echo "claude-mcp: failed to update $config; left unchanged" >&2
          fi
        else
          ${pkgs.jq}/bin/jq -n --argjson servers "$servers" \
            '{mcpServers: $servers}' > "$config"
          chmod 600 "$config"
        fi
      '';
    };
  }
