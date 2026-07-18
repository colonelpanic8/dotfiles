---
name: agent-workspace-linux
description: Drive an isolated, hidden Linux desktop and browser from the shell via the `agent-workspace-linux` CLI, without touching or stealing focus from the real desktop. Use when you need to run GUI apps or a browser as an agent-owned side workspace (computer-use style) while the user keeps working.
---

# Agent Workspace (hidden desktop) via `agent-workspace-linux`

`agent-workspace-linux` starts a hidden X11 desktop (Xvfb + openbox) that you drive
over a CLI: launch apps, screenshot, click, type, scroll, manage windows, and drive a
browser. It never touches the user's real Hyprland/Wayland session or steals focus.

Installed via Nix on desktop hosts (see `nixos/packages/agent-workspace-linux/` and
`nixos/agent-workspace.nix`), so the binary and all its runtime deps (Xvfb, openbox,
xdotool, imagemagick, xclip, bubblewrap, chromium) are on `PATH`. No MCP server —
this skill is the interface.

Every subcommand emits JSON on stdout. All operations take `--id ID` to address a
specific workspace; omit it when you only run one.

## Readiness

```bash
agent-workspace-linux doctor        # JSON readiness report; check .ready_for_x11_workspace
```

## Lifecycle

```bash
# Start a workspace. --ack-hidden-workspace is REQUIRED (acknowledges it's hidden).
agent-workspace-linux workspace start --ack-hidden-workspace \
  --id work --purpose "what this workspace is for" [--width 1280] [--height 720]

agent-workspace-linux workspace list                 # all workspaces
agent-workspace-linux workspace status --id work     # status JSON (display, size, apps)
agent-workspace-linux workspace stop --id work        # stop one workspace
agent-workspace-linux workspace cleanup --id work     # remove leftover runtime state
```

## The perceive → act loop

There is no inline image channel — screenshot to a PNG, then open it with your file
reader (the Read tool renders PNGs), decide, then act. Repeat.

```bash
# Capture the whole workspace (or a single window)
agent-workspace-linux workspace screenshot --id work --output /tmp/aw.png
agent-workspace-linux workspace screenshot-window --id work --app firefox --output /tmp/aw.png

# One call for status + optional screenshot + recent events:
agent-workspace-linux workspace observe --id work --screenshot --output /tmp/aw.png --events
```

Read `/tmp/aw.png`, then act. Coordinates are display-global for `click`/`type`/etc.,
or window-relative for the `*-window` variants.

```bash
agent-workspace-linux workspace click --id work X Y           # --button N, --count N (double-click)
agent-workspace-linux workspace move-pointer --id work X Y
agent-workspace-linux workspace type --id work "text to type"
agent-workspace-linux workspace key --id work Return          # xdotool key syntax: ctrl+a, Tab, Escape...
agent-workspace-linux workspace scroll --id work X Y down     # up|down|left|right, --amount N
agent-workspace-linux workspace drag --id work FROMX FROMY TOX TOY
```

Window-scoped equivalents avoid guessing global coordinates — target by window and use
window-local X Y:

```bash
agent-workspace-linux workspace click-window --id work --app firefox X Y
agent-workspace-linux workspace type-window  --id work --title "Save As" "filename.txt"
```

## Launching apps

```bash
# Everything after -- is the command run inside the workspace.
agent-workspace-linux workspace launch --id work --wait-window -- firefox https://example.com
agent-workspace-linux workspace launch --id work -- xterm
agent-workspace-linux workspace apps --id work                # list apps (id/pid/name)
agent-workspace-linux workspace wait-window --id work --app firefox --timeout-ms 10000
agent-workspace-linux workspace kill-app --id work firefox
```

## Windows

```bash
agent-workspace-linux workspace windows --id work                       # list; match --title/--class/--pid/--app
agent-workspace-linux workspace focus-window  --id work --app firefox
agent-workspace-linux workspace move-window   --id work WINDOW_ID X Y
agent-workspace-linux workspace resize-window --id work WINDOW_ID W H
agent-workspace-linux workspace raise-window  --id work WINDOW_ID
```

## Browser helpers (Chrome/Chromium DevTools, no pixel-guessing)

When the app is a Chromium-based browser, these read/drive it structurally instead of
by screenshot:

```bash
agent-workspace-linux workspace browser-navigate --id work --app chromium https://example.com
agent-workspace-linux workspace browser-snapshot --id work --app chromium --max-text-chars 4000
agent-workspace-linux workspace browser-search-results --id work --app chromium
```

## Clipboard

The workspace has its own clipboard (via xclip), separate from the host.

```bash
agent-workspace-linux workspace clipboard-set --id work "text"
agent-workspace-linux workspace clipboard-get --id work
agent-workspace-linux workspace paste --id work "text"          # set + paste (default Ctrl+V)
```

## Human viewer / intervention

The user can watch the hidden workspace live and take over briefly (pause / read-only /
stop controls) from a terminal on the real desktop:

```bash
agent-workspace-linux viewer --id work --always-on-top
```

## Permissions and profiles (optional)

By default the workspace runs in UI-owned mode with no enforced ceiling (network + app
launch are open). To lock it down, pass a permissions file (network mode, mount
allowlist, app allowlist) enforced for the workspace's lifetime:

```bash
agent-workspace-linux permissions template local --allow-host localhost:3000 \
  --mount "$HOME/project:/workspace/project:read_write" --app /run/current-system/sw/bin/firefox > /tmp/perm.json
agent-workspace-linux --permissions /tmp/perm.json workspace start --ack-hidden-workspace --id work
```

Reusable setups (mounts, apps to auto-launch) can be saved as profiles; see
`agent-workspace-linux profile --help` and `workspace open-profile`.

## Notes

- Always `stop` (or `cleanup`) workspaces you started so Xvfb/openbox don't linger.
- Use distinct `--id` values to run several workspaces at once.
- The workspace desktop is X11 (Xvfb); the optional `viewer` window renders fine on a
  Wayland host.
- Full command reference: `agent-workspace-linux --help`.
