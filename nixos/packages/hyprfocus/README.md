# Workspace focus mode

`hyprfocus` registers the `tabbed` tiled layout. Every tiled window remains an
independent target. Floating windows stay outside the layout. The tab strip is
reserved above the window only when there are at least two tabs.

The dotfiles bindings use Super+Ctrl+Space to toggle focus mode. Super+Space
exits to the previous layout type; Super+Shift+Space selects Columns. Arrow
focus bindings cycle tabs, and arrow swap bindings reorder them. The native
layout messages are `cyclenext`, `cycleprev`, `focustab N` (one-based), and
`movetab 1` / `movetab -1`.

Silent imports preserve the destination selection, including when the moved
window had focus on its source workspace. Explicit focus selects a tab. Closing
the selected window chooses a surviving window from focus history, then a
neighbour. A single window has no tab strip; an empty workspace remains in
focus mode until that workspace is destroyed.

## Native integration

`../../patches/hyprland-focus-targets.patch` adds the tiled algorithm capability
`acceptsGroupTargets()`. Group creation, space assignment, floating changes,
and swaps enforce this capability before changing layout membership.

| Path | Behaviour in a refusing layout |
| --- | --- |
| Toggle group, directional group creation, map-time `group set` | Creation is refused before group initialisation. Toggle checks before changing fullscreen state. |
| Workspace or monitor move | The action and global controller preflight before changing workspace, monitor, or fullscreen state. A tiled group must be ungrouped first. |
| Layout-directed transfers | `assignToSpace` refuses before removal from the old space and reports a notification. |
| Dragging a tiled group | Drag motion cannot transfer it into a space that would refuse retiling it. It returns to its source layout on drop. |
| Swaps, including tile/float exchanges | Both destination spaces validate the prospective floating states before either target changes. |
| Floating group to tiled | Refused before fullscreen or floating state changes. |
| Entering the layout | Existing tiled groups are dissolved synchronously under the old layout before iteration begins. Fullscreen state is restored on the same window. |
| Monitor removal | Whole workspaces migrate with their layout. Pinned window transfers remain floating. |

The capability changes the plugin ABI. The patch marks both the API version and
ABI hash; all configured plugins are built against the patched compositor.
Loading an unpatched plugin must fail instead of using an incompatible vtable.
The focus plugin also requires a native capability-version symbol.

The patch exposes `workspace.layout_changed` after an algorithm switch and
`workspace.destroyed` with an ID snapshot. The latter avoids the expired weak
reference in the upstream removal event. It also removes deregistered window
decorations from the plugin registry, preventing stale registrations from
accumulating across layout toggles.

## Policy and lifecycle

`dotfiles/config/hypr/hyprland/layout_policy.lua` owns per-workspace layout rules
and reserves selectors of the form `r[N-N]s[false]`. Other workspace rule fields
are unaffected. Rule handles are disabled on destruction and reused within a
Lua lifetime; reload creates fresh handles after the static rules.

An atomic, validated text file in `XDG_RUNTIME_DIR`, keyed by
`HYPRLAND_INSTANCE_SIGNATURE`, remembers layout choices and return layout types.
It contains no window IDs. Reload replays rules without flushing partial
configuration, so an unchanged effective layout retains its object, order,
and selection. Plugin unload restores visibility/input and clears stale policy
through the layout-change event. Exact previous split geometry is not restored
when leaving focus mode.

There is no membership polling or deferred group repair.

## Checks

The Nix `hyprlandConfigSyntax` check includes Lua syntax, configuration smoke
checks, and workspace policy tests. Build the compositor and configured plugins
through `hyprland-stuff.hyprlandPluginPackages` to cover the ABI change.

`tests/run.py` starts its own compositor and Ghostty clients, creates two virtual
outputs, runs `transitions.lua`, and checks reload, destruction/reuse, map rules,
IPC batches, monitor removal, unload, and close-to-empty behaviour. Pass an
absolute socket path for an **isolated** parent Wayland compositor:

```
python3 tests/run.py \
  --compositor /path/to/Hyprland --hyprctl /path/to/hyprctl \
  --plugin /path/to/libhyprfocus.so \
  --parent-wayland /path/to/isolated-wayland-socket \
  --wlrctl /path/to/wlrctl
```

Use `--overview-plugin /path/to/libhyprwinview.so` to include overview checks.
The runner keeps logs in its printed temporary directory and shuts down only
its own compositor and clients. The pinned Aquamarine nested backend requests
version 6 of `wl_compositor` and `xdg_wm_base` without negotiating the parent
version; parents offering version 5 need a test-only compatibility shim passed
with `--wayland-compat`. This is independent of the focus patch. Physical
monitor modesetting and compositor restart still require a real-session check.
