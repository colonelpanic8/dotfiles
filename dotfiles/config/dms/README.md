# Dank Material Shell

The NixOS module installs DMS (now Dux Shell) from the `assembled` branch of
`colonelpanic8/dux-shell`, the `colonelpanic8/quickshell` tray build assembled
by `~/projects/quickshell-assembly`, and the upstream DankAIUsage plugin. The
workspace and tray changes below are topic branches carried on upstream by the
fork-assembler stack in `~/projects/dux-shell-assembly`; its `BUILDING.md`
covers changing topics and releasing.
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

The fork hides special workspaces, where Hyprland scratchpads park, and shows
minimized windows in one `M` bucket per monitor, like the Taffybar
configuration. Clicking the bucket background does nothing. Clicking a
minimized window restores it to the active workspace before focusing it.
Chrome windows currently use application icons instead of tab favicons.

## Widgets and tray

Clock, media controls, audio, networking, Bluetooth, CPU temperatures, GPU
temperature, battery power, notifications, night mode, and
system controls use upstream widgets and services. Networking (with VPN and
Bluetooth) and audio are separate control center button instances, each showing
one group. CPU history with usage drawn over the graph, frequency, optional
readable RAPL package power, and a few desktop commands live in the small
DesktopExtras plugin. DesktopBrightness shows brightness as its own pill and
hides on machines without a controllable display; upstream's control center
button would show a placeholder icon instead. DesktopMemory and DesktopDisk
stack two small lines beside their icon (RAM over swap, root usage over free
space) to stay narrow on a horizontal bar; DesktopMemory shows RAM alone on
machines without swap.
Clicking the brightness pill opens sliders for the bar's display (its pinned
group, if any) and every other dimmable device; right-click opens the control
center.
AsusProfile shows asusd's platform profile and hides where asusd isn't
running; its popout sets the current, AC and battery profiles, and right-click
cycles the current one. It talks to asusd directly because strixi-minaj runs
asusd instead of power-profiles-daemon, which the upstream profile controls
need. AI
quotas use upstream DankAIUsage with remaining usage and both providers visible.
The media widget keeps the title and artist visible and shows synced lyrics
after the playback controls (the fork's "Keep title and artist" option); when
the bar runs short, the lyrics go first.

The tray orders icons by the priorities in the existing
`~/.config/taffybar/sni-priorities.yaml`, decreasing away from the screen edge.
DMS never hides tray icons; the policy's threshold and maximum visible count
only apply to Taffybar. Tray icons use the maximized bar icon size, and icons
drawn mostly in colors close to the bar background get an outline.
Right-clicking an icon shows a priority stepper; shift-scroll over an icon also
adjusts its priority. Edits preserve the shared policy file.
CrossPaste's left click opens its search window.

Each mouse button runs a configurable tray action: activate, secondary action,
menu, or nothing. Settings → bar widgets → System Tray → Click actions sets the
defaults (`systemTray{Left,Middle,Right}ClickAction`) and per-app overrides
keyed by tray item id (`systemTrayItemClickActions`, e.g.
`{"nm-applet": {"left": "menu"}}`). Activate falls back to the menu for items
that declare themselves menu-only. Menus, icon loading,
and overlay rendering use upstream DMS and the custom Quickshell build.

`~/.config/dms/settings.json` and `plugin_settings.json` are symlinks to the
files here, so changes made through the DMS settings UI are written into the
repository; commit them to share them across machines. `session.json` holds
per-machine runtime state (location, last media player, dock pins) and only
seeds `~/.local/state/dms/session.json` on first activation. The local plugins
are linked the same way, so editing their QML needs only `systemctl --user
restart dms`. The NixOS module
owns packaging, plugin registration, and systemd integration.

## Org agenda and calendar

The local `OrgAgenda` plugin shows today's org agenda from org-agenda-api,
overdue items included. It has three surfaces sharing one view: a bar pill
(right section; the island ignores `centerWidgets`) whose popout holds the
agenda, a desktop widget (instance `dw_orgAgenda`; drag it in the desktop
widget editor), and a dash card that the default dash layout leaves out.

Checkboxes complete entries in org. The capture row has a template chip that
lists every org-agenda-api capture template (`mova_capture.sh --templates`,
cached for offline use); templates with more prompts grow extra fields, and
date prompts accept `today`, `tomorrow`, weekday names, `+N` or an ISO date,
optionally with `HH:MM`. Enter moves to the next empty required field, then
queues the capture through `mova_capture.sh --values`, the same retrying queue
Hyper+T uses. The last template used is kept in the plugin's state.

The Agenda tab ends with the habits still due today. The search button swaps
the capture row for a filter over every open TODO (`mova_agenda todos`,
fetched at startup and refreshed when a search starts on data over five
minutes old); words match the title, state, category and tags.

The Habits tab (`mova_agenda habits`) lists every org-window-habit, due ones
first, with progress in each window and a strip of the last ten days, today
(taller, split into its color now and after completing) and two days ahead,
colored with org-agenda-api's habit colors. A dot marks a day with a
completion. The selected tab is kept in the plugin's state. The view runs
`mova_agenda`, which reads the API password from the agenix secret through
`mova_api_password`.

The Calendar card reads Dux Calendar (`dcal`). `nixos/dux-calendar.nix` gives
dcal the vdirsyncer Google feeds as a read-only account and the org TODOs as a
task list through org-agenda-api's `org-agenda-dcal` provider, so dated TODOs
show on their scheduled or deadline day as boxes (checked once done), each
marker kind with its count.

## Checks

```sh
cd /srv/dotfiles/nixos
nix build .#checks.x86_64-linux.dms-workspaces --no-link
```

`tests/test_integration.py` exercises shared priority edits and shell startup
failure/session fallback. It needs Python with `dbus-python` and `pyyaml`, as
provided by the module's `dms-dotfiles-state` runtime.
