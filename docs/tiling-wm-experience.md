# Tiling WM Experience Spec

This document describes the tiling window manager experience I am targeting.

## Priority Levels

- Required: daily-driver behavior.
- Important: expected for parity, but a rough first version is acceptable.
- Nice: useful polish or compatibility.

## Modifier Terminology

- `Super` names the physical modifier key often labeled Windows, Command, GUI,
  or OS depending on the keyboard.
- `Hyper` means a higher-order logical modifier layer used for monitor,
  workspace, utility, and cross-context operations.
- Prefer implementing `Hyper` as its own virtual modifier or equivalent logical
  mask when the environment supports that.
- If a dedicated virtual `Hyper` mask is not practical, `Ctrl+Alt+Super` is the
  fallback chord.
- The fallback `Hyper` chord intentionally does not include `Shift`; portable
  `Hyper` bindings only use the plain `Hyper` layer and the `Hyper+Shift`
  layer.
- Do not require `Hyper+Ctrl`, `Hyper+Alt`, or `Hyper+Super` bindings. Those
  modifiers may already be part of the fallback `Hyper` chord.
- Binding descriptions should use `Super` and `Hyper` rather than
  hardware-vendor names.

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
- Hidden/special workspaces are excluded from the status bar's normal workspace
  list.

## Directional Navigation

Required behavior:

- Directional window focus is available.
- Directional window swapping or movement is available.
- Directional move-to-monitor is available while preserving useful focus.
- Directional monitor focus is available.
- Directional window movement between monitors is available.
- Moving the focused window to an empty workspace on the monitor in a direction
  remains required behavior, but it should not require an extra `Hyper`
  modifier beyond `Shift`.
- `Super+w/a/s/d` focuses windows directionally.
- `Super+Shift+w/a/s/d` swaps or moves the focused window directionally.
- `Super+Ctrl+w/a/s/d` moves the focused window to the monitor in that
  direction while preserving useful focus.
- `Super+Ctrl+Shift+w/a/s/d` moves the focused window to an empty workspace on
  the monitor in that direction.
- `Hyper+w/a/s/d` focuses monitors directionally.
- `Hyper+Shift+w/a/s/d` swaps or moves windows between monitors directionally.
- Directional focus in tabbed/fullscreen mode should cycle predictably through
  windows even though their screen geometry overlaps.

Important behavior:

- Keyboard resize remains available, but it should not displace the directional
  move-to-monitor binding.

## Layouts

Required behavior:

- Tiling is dynamic.
- Primary layout is equal-width vertical columns.
- Scrolling layouts are not acceptable.
- All ordinary splits are vertical.
- Adding windows dynamically redistributes all tiled windows evenly.
- Removing windows dynamically redistributes all tiled windows evenly.
- Ordinary use should not require manually managing a split tree.
- Tabbed/fullscreen-style monocle layout is available.
- Directional window navigation bindings continue to switch windows in
  tabbed/fullscreen mode.
- The important layouts are columns and tabbed/fullscreen.
- Dialogs float.
- Dialogs are centered.
- There is a command to jump directly to the columns layout and one to jump
  directly to the tabbed/fullscreen layout.
- Layout state is per workspace when the compositor supports it.

Important behavior:

- One-window workspaces should have no visible gaps or use smart gaps.

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

- Overview supports both "go" and "bring" workflows.
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

## Status Bar Contract

Required behavior:

- The status bar can list normal workspaces.
- The status bar can identify the active workspace per monitor.
- The status bar can list windows per workspace.
- The status bar can expose class hints for each listed window.
- The status bar can expose title for each listed window.
- The status bar can expose active state for each listed window.
- The status bar can expose minimized state when available.
- The status bar can expose urgency when available.
- The status bar can expose approximate window position when available.
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
- The status bar can be toggled per monitor.
- Session startup integrates with the normal graphical-session target.
- Session startup integrates with any required session-specific user target.

Nice behavior:

- Wallpaper behavior remains consistent.
- Wallpaper selection uses `Hyper+comma`; `Hyper+w/a/s/d` are reserved for
  directional monitor focus.
- Idle behavior remains consistent.
- Lock behavior remains consistent.
- Clipboard history behavior remains consistent.
- Screenshot behavior remains consistent.
- Monitor DDC/input switching remains consistent.
- Rofi utility bindings remain consistent.
- Media keys remain consistent.

## Binding Appendix

Required behavior:

- `Hyper` bindings should remain available from a single physical key where
  practical, even if that key emits the fallback chord internally.
- Extra modifiers on `Hyper` are limited to `Shift` for portable bindings.

Important behavior:

- `Hyper` utility bindings must not displace required directional monitor
  bindings on `Hyper+w/a/s/d`.

### Core Bindings

Required behavior:

- `Super+p` opens the application launcher.
- `Super+Shift+p` opens the run menu.
- `Super+Shift+Return` opens a terminal.
- `Super+q` reloads the window manager config.
- `Super+Shift+c` closes the focused window.
- `Super+Shift+q` exits the window manager session.
- `Super+Tab` opens the overview.
- `Super+Shift+Tab` opens the overview in bring-window mode when supported.
- `Super+g` opens the go-to-window picker.
- `Super+b` opens the bring-window picker.
- `Super+Shift+b` opens the replace-window picker.
- `Super+\` toggles to the previous workspace on the current monitor.
- `Super+Shift+e` moves the focused window to the next empty workspace and
  follows it. This is the target replacement for the older `Super+Shift+h`
  binding.
- `Hyper+e` focuses the next empty workspace.
- `Hyper+5` swaps the current workspace with a selected workspace.
- `Hyper+g` gathers windows of the focused class onto the current workspace.

### Directional Navigation Bindings

Required behavior:

- `Super+w/a/s/d` focuses windows directionally.
- `Super+Shift+w/a/s/d` swaps or moves the focused window directionally.
- `Super+Ctrl+w/a/s/d` moves the focused window to the monitor in that
  direction while preserving useful focus.
- `Hyper+w/a/s/d` focuses monitors directionally.
- `Hyper+Shift+w/a/s/d` swaps or moves windows between monitors directionally.
- Moving the focused window to an empty workspace on the monitor in a direction
  remains required behavior, but it should not require a `Hyper+Ctrl` binding.
- `Super+z` focuses the next monitor.
- `Super+Shift+z` moves the focused window to the next monitor.

### Numbered Workspace Bindings

Required behavior:

- `Super+1..9` focuses workspace `1..9` on the current monitor.
- `Super+Shift+1..9` sends the focused window to workspace `1..9` without
  following it.
- `Super+Ctrl+1..9` sends the focused window to workspace `1..9` and follows
  it.

### Scratchpad Bindings

Required behavior:

- `Super+Alt+e` toggles the element scratchpad.
- `Super+Alt+g` toggles the gmail scratchpad.
- `Super+Alt+h` toggles the htop scratchpad.
- `Super+Alt+m` toggles the messages scratchpad.
- `Super+Alt+k` toggles the slack scratchpad.
- `Super+Alt+s` toggles the spotify scratchpad.
- `Super+Alt+t` toggles the transmission scratchpad.
- `Super+Alt+v` toggles the volume scratchpad.

Important behavior:

- `Super+Alt+grave` toggles the dropdown terminal scratchpad.
- `Super+Alt+c` raises or starts the browser.
- `Super+Alt+Return` enters the minimized-window picker or restores minimized
  windows, depending on environment support.
- `Super+Alt` is reserved for app-specific raise/spawn, scratchpad, and
  scratchpad-adjacent bindings.

### Utility Bindings

Required behavior:

- `Hyper+v` opens clipboard history with a rofi-backed clipboard command
  such as `greenclip print` or `cliphist`.
- `Hyper+p` opens the password picker with `rofi-pass`.
- `Hyper+h` opens the screenshot tool with the compositor/session-appropriate
  screenshot command.
- `Hyper+c` opens a shell command prompt with `shell_command.sh`.
- `Hyper+x` opens the command picker with `rofi_command.sh`.
- `Hyper+k` opens the process killer with `rofi_kill_process.sh`.
- `Hyper+Shift+k` opens the kill-all/process-tree killer with
  `rofi_kill_all.sh`.
- `Hyper+r` opens the systemd/service menu with `rofi-systemd`.
- `Hyper+slash` toggles the status bar with the status-bar-appropriate command.
- `Hyper+backslash` toggles the monitor input with `mpg341cx_input toggle`.
- `Hyper+i` opens the audio input selector with `rofi_select_input.hs`.
- `Hyper+o` opens the audio output selector with `rofi_paswitch`.
- `Hyper+y` opens the agentic skill picker with `rofi_agentic_skill`.
- `Hyper+Shift+l` locks the session with the compositor/session-appropriate
  locker.

Important behavior:

- Wallpaper selection is available under `Hyper` via `rofi_wallpaper.sh`, but
  its exact key must avoid the required `Hyper+w/a/s/d` directional monitor
  bindings.
- Expose-style overview remains available as a utility binding using the
  compositor-appropriate implementation.
- Session-destructive operations use shifted or otherwise harder-to-hit
  variants.
