# Tiling WM Experience Spec

This document describes the tiling window manager experience I am targeting.

## Priority Levels

- Required: daily-driver behavior.
- Important: expected for parity, but a rough first version is acceptable.
- Nice: useful polish or compatibility.

## Workspaces and Monitors

Required behavior:

- Workspaces are a shared global set, not independent per-monitor namespaces.
- Focusing workspace `N` shows workspace `N` on the currently focused monitor.
- Moving a window to workspace `N` does not require caring which monitor
  currently owns that workspace.
- Sending the focused window to workspace `N` without following it is a
  first-class operation.
- Moving the focused window to workspace `N` and following it is a first-class
  operation.
- Sending the focused window to the next empty workspace without following it is
  a first-class operation.
- Moving the focused window to the next empty workspace and following it is a
  first-class operation.
- Normal workspaces are bounded to `1..9`.
- Workspace history is tracked per monitor.
- Last-workspace toggle uses the current monitor's workspace history.
- Workspace cycling works on the current monitor within the bounded workspace
  set.

Important behavior:

- Swapping the current workspace contents with another workspace is available.
- Moving a window to an empty workspace on another monitor is available.
- Moving the focused window to another monitor without following keeps keyboard
  focus on the original monitor.
- Moving the focused window to another monitor and following it moves keyboard
  focus to the destination monitor.
- Hidden/special workspaces exist for scratchpad state.
- Hidden/special workspaces exist for minimized state.
- Hidden/special workspaces are excluded from ordinary workspace cycling.
- Hidden/special workspaces are excluded from taffybar's normal workspace list.

## Directional Navigation

Required behavior:

- `Super+w/a/s/d` focuses windows directionally.
- `Super+Shift+w/a/s/d` swaps or moves the focused window directionally.
- `Super+Ctrl+w/a/s/d` moves the focused window to the monitor in that
  direction while preserving useful focus.
- `Hyper+w/a/s/d` focuses monitors directionally.
- `Hyper+Shift+w/a/s/d` swaps or moves windows between monitors directionally.
- `Hyper+Ctrl+w/a/s/d` moves the focused window to an empty workspace on the
  monitor in that direction.

Important behavior:

- Directional focus in tabbed/fullscreen mode should cycle predictably through
  windows even though their screen geometry overlaps.
- Keyboard resize remains available, but it should not displace the directional
  move-to-monitor binding.

## Layouts

Required behavior:

- Tiling is dynamic.
- Primary layout is equal-width vertical columns.
- All ordinary splits are vertical.
- Adding windows dynamically redistributes all tiled windows evenly.
- Removing windows dynamically redistributes all tiled windows evenly.
- Ordinary use should not require manually managing a split tree.
- Tabbed/fullscreen-style monocle layout is available.
- Directional window navigation bindings continue to switch windows in
  tabbed/fullscreen mode.
- The important layouts are columns and tabbed/fullscreen.

Important behavior:

- One-window workspaces should have no visible gaps or use smart gaps.
- Gaps are small in normal multi-window layouts.
- Dialogs float.
- Dialogs are centered.
- Layout state is per workspace when the compositor supports it.
- There is a command to jump directly to the columns layout and one to jump
  directly to the tabbed/fullscreen layout.

Nice behavior:

- Gaps can be toggled.
- Fullscreen can be toggled.
- Smart borders can be toggled.
- Layout-related modifiers remain available for experiments.
- Inactive windows are slightly dimmed when supported.

## Overview and Discovery

Required behavior:

- There is an expose-style way to inspect open windows or workspaces before
  jumping.
- There is a rofi-style window picker.
- Window picker entries show icons.
- Window picker entries show titles.
- Window picker entries show workspace labels.
- Go-to-window focuses the selected window wherever it currently lives.
- Bring-window moves a selected non-visible window to the current workspace and
  focuses it.
- Replace-window swaps the focused window with a selected window where feasible.

Important behavior:

- Overview supports both "go" and "bring" workflows when possible.
- Window switchers hide scratchpad windows unless the user is explicitly using a
  scratchpad picker.
- Window switchers hide minimized windows unless the user is explicitly using a
  minimized picker.
- Window switchers hide internal windows.
- Go/bring actions unminimize selected windows when needed.

## Scratchpads

Required behavior:

- A named scratchpad exists for element.
- A named scratchpad exists for gmail.
- A named scratchpad exists for htop.
- A named scratchpad exists for messages.
- A named scratchpad exists for slack.
- A named scratchpad exists for spotify.
- A named scratchpad exists for transmission.
- A named scratchpad exists for volume.
- Scratchpads appear near-fullscreen and centered by default.
- Toggling a scratchpad deactivates fullscreen/tabbed state first.
- Scratchpads are hidden from normal workspace and window listings.

Important behavior:

- A dropdown terminal scratchpad exists.
- Scratchpad matching handles delayed class/title assignment.
- Scratchpad behavior is robust when the app is already running.
- Scratchpad behavior is robust when the app is minimized.
- Scratchpad behavior is robust when the app is on another workspace.

## Minimization

Required behavior:

- Focused window can be minimized.
- Last minimized window can be restored to the current workspace and focused.
- Minimized windows are excluded from normal layout.
- Minimized windows are excluded from ordinary go/bring lists.

Important behavior:

- A minimized picker mode exists.
- Restore-all-minimized exists.
- Other classes in the current workspace can be minimized.
- Windows of the focused class can be restored.
- All minimized windows can be restored.

## Class-Aware Workflows

Important behavior:

- Gather all windows of the focused class onto the current workspace.
- Raise-or-spawn exists for the browser.
- Window menus show class.
- Window menus show title.
- Window menus show workspace.
- Window menus show icon.

## Taffybar Contract

Required behavior:

- Taffybar can list normal workspaces.
- Taffybar can identify the active workspace per monitor.
- Taffybar can list windows per workspace.
- Taffybar can expose class hints for each listed window.
- Taffybar can expose title for each listed window.
- Taffybar can expose active state for each listed window.
- Taffybar can expose minimized state when available.
- Taffybar can expose urgency when available.
- Taffybar can expose approximate window position when available.
- Scratchpad workspaces are marked as special or filtered out.
- Minimized workspaces are marked as special or filtered out.
- Internal workspaces are marked as special or filtered out.

Important behavior:

- Workspace labels are stable.
- Workspace icons are stable.
- Window positioning information is available enough for workspace icon strips
  and future expose-like views.
- Layout information is available enough for workspace icon strips and future
  expose-like views.
- Layout name is exposed if practical.
- Layout state is exposed if practical.

## Session and Utility Behavior

Important behavior:

- Terminal is `ghostty --gtk-single-instance=false`.
- Launcher is `rofi -show drun -show-icons`.
- Run menu is `rofi -show run`.
- Browser raise/spawn behavior exists.
- Border width is effectively zero.
- Taffybar can be toggled per monitor.
- Session startup integrates with the normal graphical-session target.
- Session startup integrates with any required session-specific user target.

Nice behavior:

- Wallpaper behavior remains consistent.
- Idle behavior remains consistent.
- Lock behavior remains consistent.
- Clipboard history behavior remains consistent.
- Screenshot behavior remains consistent.
- Monitor DDC/input switching remains consistent.
- Rofi utility bindings remain consistent.
- Media keys remain consistent.
