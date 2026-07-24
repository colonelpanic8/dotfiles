---
name: pair-t3code-remote
description: Pair the T3 Code desktop client on this machine with a remote T3 Code execution environment, preferably end-to-end through SSH. Use when asked to add, connect, pair, repair, or verify a remote T3 Code server or SSH-launched environment in the local T3 Code client.
---

# Pair T3 Code with a Remote Environment

Complete the pairing autonomously when existing SSH credentials permit it. Prefer T3 Code's
desktop-managed SSH launch because it bootstraps or reuses the remote server, opens a local
forward, mints the pairing credential, and stores the resulting client secret with Electron safe
storage.

## Guardrails

- Treat pairing URLs, pairing tokens, and bearer sessions as secrets. Never include them in the
  final response, shell tracing, logs, or committed files.
- Do not edit `~/.local/share/t3code/userdata/connection-catalog.json` or
  `saved-environments.json` directly. Current catalogs and secrets may be encrypted.
- Do not expose a server to the public Internet, change firewall rules, alter Tailscale Serve, or
  rebuild a remote host merely to complete pairing unless the user authorized that expansion.
- Do not create a duplicate saved environment. Inspect the Connections UI and test an existing
  entry for the same host first.
- Do not stop an existing T3 Code server or another SSH-managed session unless repairing that
  exact environment requires it.
- Use short-lived pairing credentials, normally 15 minutes. A successful exchange creates the
  longer-lived client session.

## Discover the Target

1. Use an exact SSH target supplied by the user when available.
2. Otherwise inspect `~/.ssh/config`, `ssh -G <alias>`, and `tailscale status` for likely targets.
3. If multiple plausible hosts remain, ask which host to use. Do not guess.
4. Resolve the effective host, user, port, identity files, and proxy settings with:

```bash
ssh -G TARGET
```

5. Run the bundled read-only preflight:

```bash
scripts/t3code-remote-pair probe TARGET
```

The preflight verifies non-interactive SSH, reports Node/T3 availability, checks the user service,
and reports Tailnet addresses without creating credentials or changing either machine.

If an unscoped link-local IPv6 result makes a dual-stack hostname fail intermittently, force the
working family without changing SSH configuration:

```bash
T3CODE_SSH_ADDRESS_FAMILY=inet scripts/t3code-remote-pair probe TARGET
```

When the working address is a Tailscale FQDN but the SSH host key is stored under a short alias,
preserve that known-hosts identity while using the resolvable target:

```bash
T3CODE_SSH_ADDRESS_FAMILY=inet \
T3CODE_SSH_HOST_KEY_ALIAS=SHORT_ALIAS \
  scripts/t3code-remote-pair probe USER@HOST.TAILNET.ts.net
```

If key authentication fails, search `pass` for the host before asking the user. Never place a
password on an SSH command line. T3 Code's desktop SSH flow can present an askpass prompt when a
password is genuinely required.

## Choose the Pairing Path

### Prefer desktop-managed SSH

Use this when SSH works and the user does not specifically require an already-running persistent
server or a directly reachable HTTPS/LAN endpoint.

The remote host needs Node `^22.16 || ^23.11 || >=24.10`. The desktop launcher searches common
system, Nix, Volta, asdf, mise, fnm, nodenv, and nvm locations. If preflight cannot see a compatible
Node, inspect the remote non-interactive shell before changing it:

```bash
ssh TARGET 'sh -lc "command -v node && node --version"'
```

Then automate the local desktop application:

1. Read `/srv/dotfiles/dotfiles/agents/skills/computer-use-linux/SKILL.md` completely.
2. Use Computer Use to find or launch the packaged T3 Code desktop window.
3. Open **Settings** → **Connections**.
4. Check for an existing environment matching the SSH alias/hostname.
5. Choose **Add environment** → the SSH launch flow.
6. Enter the SSH target exactly as validated and confirm.
7. Handle an SSH askpass prompt only with a credential retrieved from `pass` or supplied by the
   user.
8. Wait for the environment to be saved and connected.

Do not separately create a token for this path. The desktop launcher runs the remote pairing
command and saves the result itself.

### Pair an existing persistent server

Use this when the remote server is already managed independently and has a reachable endpoint.
Prefer its verified Tailscale HTTPS/MagicDNS URL. A loopback-only endpoint cannot be directly
paired from another machine; use desktop-managed SSH instead.

1. Confirm the exact server state directory. Ivan's dotfiles-managed service exports
   `T3CODE_HOME=$HOME/.t3`, so use `--base-dir "$HOME/.t3"`.
2. Confirm the public base URL reaches that same server. Do not infer that a Tailnet IP and port are
   exposed just because they exist.
3. Create a short-lived pairing link into a mode-0600 temporary file:

```bash
pair_file="$(mktemp)"
chmod 600 "$pair_file"
scripts/t3code-remote-pair create TARGET HTTPS_BASE_URL >"$pair_file"
```

4. Read the computer-use skill as above, open **Settings** → **Connections**, and add a remote
   environment using the full pairing URL.
5. Verify successful connection, then remove the temporary file by its exact path.

The helper defaults to a 15-minute token and `$HOME/.t3`. Use its `--help` for an alternate TTL,
label, or remote base directory.

## Local and Personal NixOS Conventions

- The packaged executable is normally `t3code-desktop`; the CLI is `t3`.
- Packaged state defaults to `~/.t3/userdata`; the legacy desktop user-data directory under
  `~/.config/t3code` is not the connection catalog.
- Dotfiles-managed headless servers use `t3code-headless.service`, loopback port 3774 by default,
  and optional Tailscale Serve. Machine overrides may choose another HTTPS port.
- Inspect remote service state with `systemctl --user status t3code-headless.service` and
  `systemctl --user show t3code-headless.service -p ExecStart -p ActiveState`.
- If the remote is managed by `/srv/dotfiles`, repair configuration through its existing Nix
  module and activate only with `just switch` from that remote's `/srv/dotfiles/nixos`. Pairing
  alone does not authorize a rebuild.

## Verify

Require all applicable checks:

1. The Connections UI shows one saved entry for the intended environment.
2. Its status is connected, not merely saved.
3. The environment picker exposes it and remote projects/threads load.
4. For SSH launch, the local forward remains active and reconnects after navigating away and back.
5. For direct pairing, the advertised HTTP/WS endpoint corresponds to the intended host.
6. No pairing token appears in the final response, retained temp files, or command history.

If version skew prevents connection, report the local and remote versions and use T3 Code's
offered update/reconnect action only when it affects a launcher-managed server. Do not silently
replace or restart an independently managed persistent server.

## Handoff

Report:

- the saved environment label and SSH alias/hostname;
- whether the connection uses desktop-managed SSH or a direct endpoint;
- whether the connection and environment data were verified;
- any remaining remote prerequisite or version mismatch.

Never report the pairing credential or bearer token.
