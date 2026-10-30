# Dank Material Shell

The NixOS module installs upstream DMS (now Dux Shell), the pinned
`colonelpanic8/quickshell` tray build, and the upstream DankAIUsage plugin.
DMS is the default Hyprland shell. Taffybar remains installed. In a Hyprland
session:

```sh
desktop_shell_ui set dms
desktop_shell_ui set taffybar
desktop_shell_ui toggle
```

The selection persists across logins. X11 and Plasma sessions use Taffybar.
A failed DMS startup restores Taffybar. Switching also hands notifications and
color temperature between rumno/wlsunset and DMS. The existing window picker
and its go/bring/replace actions keep working through `hypr_shell_ui`.

## Workspaces

The upstream workspace widget shows individual window icons without grouping,
highlights the active application, and filters to occupied workspaces plus the
active workspace on each monitor. Clicking an icon focuses its window;
clicking the workspace background switches workspaces. Hover and drag behavior
come from upstream.

The small carried patch groups special-workspace windows into one `M` bucket
per monitor, like the Taffybar configuration. Clicking the bucket background
does nothing. Clicking an actually minimized window restores it to the active
workspace before focusing it. Other scratchpad windows use upstream focus.
Chrome windows currently use application icons instead of tab favicons.

## Widgets and tray

Clock, media controls, audio, brightness, networking, Bluetooth, CPU usage and
temperatures, GPU temperature, RAM/swap, disk space, battery power, notifications,
night mode, and system controls use upstream widgets and services. CPU history,
frequency, optional readable RAPL package power, and a few desktop commands live
in the small DesktopExtras plugin. AI quotas use upstream DankAIUsage with
remaining usage and both providers visible.

The tray reads the existing `~/.config/taffybar/sni-priorities.yaml`, including
its collapse threshold and maximum visible icons. Hover expands hidden icons.
Shift-scroll over an icon adjusts its priority and preserves the shared policy
file. LocalSend's left click opens the existing scratchpad. Menus, icon loading,
and overlay rendering use upstream DMS and the custom Quickshell build.

`settings.json`, `plugin-settings.json`, and `session.json` seed writable user
configuration on first activation. Changes made through DMS survive restarts and
rebuilds. The NixOS module owns packaging, plugin registration, and systemd
integration. The upstream shell
is packaged separately from the carried QML patch so configuration edits do not
recompile its Go backend.

## Checks

```sh
cd /srv/dotfiles/nixos
nix build .#checks.x86_64-linux.dms-workspaces --no-link
```

`tests/test_integration.py` exercises shared priority edits and shell startup
failure/session fallback. It needs Python with `dbus-python` and `pyyaml`, as
provided by the module's `dms-dotfiles-state` runtime.
