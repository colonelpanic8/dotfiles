# Hyprland Lua Migration Checklist

This checklist tracks the migration described in `docs/tiling-wm-experience.md`.

Guiding rule for shelling out:

- Prefer Lua for compositor/window/workspace state changes.
- Avoid `hyprctl` for window manipulation unless there is no usable Lua API.
- `hyprctl` remains acceptable for non-window-control escape hatches such as
  `hyprctl reload`.
- External utilities remain acceptable where they are the real tool being
  launched, for example rofi, cliphist, grim/slurp/swappy, playerctl, hyprlock,
  and systemd commands.

## 0. Version And Build Base

- [x] Update/confirm Hyprland Lua input at latest usable upstream target.
- [x] Keep stable Hyprland path intact until Lua path is proven.
- [x] Keep hy3 out of the Lua branch.
- [x] Keep hyprNStack following the Lua Hyprland input.
- [x] Rebuild hyprNStack against the Lua Hyprland branch.
- [x] Add a forked hyprexpo input for the Lua Hyprland branch.
- [x] Keep a cheap Lua check: parse config, execute against stub, reject
      `hyprctl` in the Lua config's window/workspace manipulation path.
- [x] Add a real Hyprland Lua verifier check for the config parser path.

Current upstream note: latest Hyprland release observed during this migration is
`v0.54.3`; the Lua config input tracks PR 13817 and was already at the current
PR head `c35a8a5` dated 2026-04-26. The non-Lua fallback remains pinned to the older
hy3/hyprexpo-compatible stack; the Lua branch uses forked hyprexpo branch
`colonelpanic8/hyprland-plugins:hyprexpo-lua-hyprland`.

## 1. Core Layout

- [x] Primary layout is equal-width columns.
- [x] No scrolling layout.
- [x] No hy3 in Lua path.
- [x] Dynamic redistribution on open/close via Lua-managed nStack count.
- [x] Monocle/tabbed-style layout available.
- [x] Direct jump to columns layout.
- [x] Direct jump to monocle layout.
- [x] Directional focus cycles in monocle.
- [x] Visual indication of hidden monocle windows, currently notification.
- [x] Make layout state per workspace instead of one global current layout.
- [x] Preserve one-window smart gaps in the live config path.
- [x] Use a persistent monocle indicator instead of a transient notification.

Smart-gaps note: nStack uses `no_gaps_when_only = true`; Hyprland workspace
rules are still applied at runtime for broader parity, but skipped during
`--verify-config` because the current Lua PR segfaults when rule bindings run in
verifier mode.

## 2. Workspace Behavior

- [x] `Super+1..9` focuses bounded workspaces.
- [x] `Super+Shift+1..9` sends window without following.
- [x] `Super+Ctrl+1..9` sends and follows.
- [x] Previous workspace per monitor uses Lua-tracked history.
- [x] Implement next empty workspace focus in Lua.
- [x] Implement move focused window to next empty workspace without following.
- [x] Implement move focused window to next empty workspace and follow.
- [x] Implement bounded workspace cycling `1..9` in Lua, replacing
      `workspace-scroll.sh`.
- [x] Implement workspace swap or decide whether native dispatcher is enough.
- [x] Track current monitor workspace history explicitly, with native
      `previous_per_monitor` as fallback.

## 3. Directional Navigation

- [x] `Super+w/a/s/d` focuses windows.
- [x] `Super+Shift+w/a/s/d` swaps windows.
- [x] `Hyper+w/a/s/d` focuses monitors.
- [x] `Hyper+Shift+w/a/s/d` moves windows to monitors.
- [x] `Super+z` next monitor.
- [x] `Super+Shift+z` move to next monitor.
- [x] Replace any old cursor-follow/move scripts fully.
- [x] Add required `Super+Ctrl+w/a/s/d` move-to-monitor behavior preserving
      useful focus.
- [x] Add "move to empty workspace on monitor in direction" without requiring
      `Hyper+Ctrl`.
- [x] Route directional focus in monocle through deterministic Lua cycling.
- [ ] Live-verify directional focus in monocle behaves predictably.

## 4. Script Elimination Priority

- [x] Core layout switching no longer uses scripts.
- [x] Core column count logic no longer uses scripts or `hyprctl`.
- [x] Replace `find-empty-workspace.sh`.
- [x] Replace `workspace-goto-empty.sh`.
- [x] Replace `workspace-move-to-empty.sh`.
- [x] Replace `workspace-scroll.sh`.
- [x] Replace `cycle-layout.sh`.
- [x] Replace `movewindow-follow-cursor.sh`.
- [x] Replace `gather-class.sh`.
- [x] Replace `focus-next-class.sh`.
- [x] Replace `raise-or-run.sh`.
- [x] Replace minimize scripts if Lua can maintain hidden workspace state.
- [x] Replace `swap-workspaces.sh`.
- [x] Decide whether rofi-backed pickers remain scripts or become
      Lua-generated command pipes. Rofi itself remains external.

## 5. Overview And Window Discovery

- [x] Restore visual hyprexpo for `Super+Tab` overview.
- [x] Restore visual hyprexpo `bring` mode for `Super+Shift+Tab`.
- [x] Keep first-pass Lua numbered window picker on secondary bindings.
- [x] Implement first-pass Lua-native go-to-window picker.
- [x] Implement first-pass Lua-native bring-window picker.
- [x] Implement first-pass Lua-native replace-window picker.
- [ ] Picker entries include icons.
- [x] Picker entries include title/workspace.
- [x] Hide scratchpad/minimized/internal windows from normal pickers.
- [x] Decide whether picker data generation can be Lua-native with rofi as only
      external process.

Picker decision: current Lua API can query and manipulate windows directly, but
does not expose a synchronous way to run rofi and consume its selected output.
The first pass therefore uses Lua-native numbered submaps and notifications.
A final rofi/icon picker would need either a small IPC bridge or an upstream Lua
process-output/callback primitive.

Hyprexpo decision: hyprexpo is kept as the visual overview. The forked Lua
branch exposes `hl.plugin.hyprexpo.expo(...)`, so the Lua config can invoke
`toggle` and `bring` directly without shelling out to `hyprctl`.

## 6. Scratchpads

- [x] Preserve named scratchpads: element, gmail, htop, messages, slack,
      spotify, transmission, volume.
- [x] Preserve dropdown terminal scratchpad.
- [x] Scratchpads near-fullscreen and centered.
- [x] Scratchpads hidden from normal listings/status bar.
- [x] Toggling scratchpad exits fullscreen/monocle state first.
- [x] Decide hyprscratch daemon is not needed in the Lua branch.
- [x] Replace `hyprscratch toggle` with Lua-managed scratchpad toggles.
- [x] Disable hyprscratch service on the Lua branch.
- [x] Handle delayed class/title assignment with window class/title event adoption.
- [x] Handle already-running app.
- [x] Handle minimized app.
- [x] Handle app on another workspace.

## 7. Minimization

- [x] Implement minimize active window.
- [x] Implement restore last minimized window.
- [x] Exclude minimized windows from layout.
- [x] Exclude minimized windows from normal go/bring lists.
- [x] Implement minimized picker.
- [x] Implement restore all minimized.
- [x] Implement minimize other windows of current workspace class.
- [x] Implement restore windows of focused class.
- [x] Decide hidden workspace naming/state model for minimized windows.

## 8. Class-Aware Workflows

- [x] Gather all windows of focused class onto current workspace.
- [x] Focus next window of different/same class as desired parity.
- [x] Browser raise-or-spawn.
- [x] Window info command exposes class/title/workspace/address/pid.
- [ ] Window menus expose real window icons.
- [x] Prefer Lua window queries over `hyprctl clients`.

## 9. Status Bar Contract

- [ ] Confirm taffybar can still list normal workspaces.
- [ ] Confirm special scratchpad/minimize workspaces are filtered.
- [ ] Confirm active workspace per monitor remains visible.
- [ ] Confirm class/title/active/minimized/urgent metadata is available.
- [x] Expose layout name/state if practical.
- [ ] Confirm workspace/window positioning remains enough for icon strips.

Layout state note: Lua writes `$XDG_RUNTIME_DIR/hyprland-layout-state` with the
active workspace, active layout, and per-workspace layout map. Taffybar still
needs a live readback check.

## 10. Session And Utilities

- [x] Terminal binding preserved.
- [x] Launcher/run menu preserved.
- [x] Media keys preserved.
- [x] Clipboard history binding preserved.
- [x] Screenshot binding preserved.
- [x] Lock binding preserved.
- [x] Session startup target integration preserved.
- [x] `hyprctl reload` may remain available as a non-window-manipulation escape
      hatch.
- [x] Resolve `Hyper+w` conflict: monitor focus must win; wallpaper picker
      needs another key.
- [x] Keep rofi utility commands as external commands unless there is a
      meaningful Lua replacement.
- [x] Decide which shell utilities are acceptable because they are not Hyprland
      control scripts.

## 11. Validation

- [x] Lua syntax check.
- [x] Lua stub execution check.
- [x] `hyprctl` rejection in Lua config for window/workspace manipulation.
- [x] Real `Hyprland --verify-config` check.
- [x] hyprNStack flake build check.
- [x] hyprexpo Lua-branch flake build check.
- [x] `ryzen-shine` system dry-run.
- [x] Re-run checks after Hyprland/Lua input confirmation.
- [ ] Try live compositor smoke test again after version bump.
- [x] Document `--verify-config` caveats for Lua rule/plugin-specific config.
- [ ] Eventually run `just switch` only when the branch is coherent enough for a
      live test.

Live-smoke note: this Hyprland binary exposes `--verify-config` but no
`--headless` CLI flag. A true compositor smoke test still needs either a nested
Wayland session that avoids startup side effects or an intentional `just switch`.
