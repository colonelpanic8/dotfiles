# Dank Material Shell

The NixOS module installs DMS (now Dux Shell) from the `dotfiles` branch of
`colonelpanic8/dux-shell`, the `colonelpanic8/quickshell` tray build assembled
by `~/projects/quickshell-assembly`, and the upstream DankAIUsage plugin. The
fork branch carries the workspace and tray changes below as commits on top of
upstream; rebase it to pick up upstream changes.
DMS is the default Hyprland shell. Taffybar remains installed. In a Hyprland
session:

```sh
desktop_shell_ui set dms
desktop_shell_ui set taffybar
desktop_shell_ui toggle
```

The selection persists across logins. X11 and Plasma sessions use Taffybar.
A failed DMS startup restores Taffybar. Switching also hands notification and
color-temperature services to the selected shell. DMS provides the volume and
brightness overlays. The existing window picker and its go/bring/replace actions
keep working through `hypr_shell_ui`.

## Workspaces

The upstream workspace widget shows individual window icons without grouping,
highlights the active application, and filters to occupied workspaces plus the
active workspace on each monitor. Clicking an icon focuses its window;
clicking the workspace background switches workspaces. Hover and drag behavior
come from upstream.

The fork hides special workspaces and the `scratch-hidden-*` scratchpad
workspaces, and shows minimized windows in one `M` bucket per monitor, like the
Taffybar configuration. Clicking the bucket background does nothing. Clicking a
minimized window restores it to the active workspace before focusing it.
Chrome windows currently use application icons instead of tab favicons.

## Widgets and tray

Clock, media controls, audio, brightness, networking, Bluetooth, CPU usage and
temperatures, GPU temperature, RAM/swap, disk space, battery power, notifications,
night mode, and system controls use upstream widgets and services. CPU history,
frequency, optional readable RAPL package power, and a few desktop commands live
in the small DesktopExtras plugin. AI quotas use upstream DankAIUsage with
remaining usage and both providers visible.

The tray orders icons by the priorities in the existing
`~/.config/taffybar/sni-priorities.yaml`, decreasing away from the screen edge.
DMS never hides tray icons; the policy's threshold and maximum visible count
only apply to Taffybar. Tray icons use the maximized bar icon size, and icons
drawn mostly in colors close to the bar background get an outline.
Right-clicking an icon shows a priority stepper; shift-scroll over an icon also
adjusts its priority. Edits preserve the shared policy file.
CrossPaste's left click opens its search window. Menus, icon loading,
and overlay rendering use upstream DMS and the custom Quickshell build.

`settings.json`, `plugin-settings.json`, and `session.json` seed writable user
configuration on first activation. Changes made through DMS survive restarts and
rebuilds. The NixOS module owns packaging, plugin registration, and systemd
integration.

## Checks

```sh
cd /srv/dotfiles/nixos
nix build .#checks.x86_64-linux.dms-workspaces --no-link
```

`tests/test_integration.py` exercises shared priority edits and shell startup
failure/session fallback. It needs Python with `dbus-python` and `pyyaml`, as
provided by the module's `dms-dotfiles-state` runtime.
