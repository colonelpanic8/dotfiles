# River Window Manager Evaluation

This evaluates window managers for the current River split-compositor model,
where River delegates policy to a client implementing
`river-window-management-v1`.

It is intentionally separate from `docs/tiling-wm-experience.md`: that document
describes the target experience, while this document evaluates current River
implementation options against it.

## Short Recommendation

The best current candidates are:

1. `kwm` if the goal is the most usable latest-protocol experiment quickly.
2. `orilla` if the goal is the closest long-term match to an XMonad-like,
   code-driven, extensible environment.
3. `rhine` if Hyprland-like IPC and directional commands are more important
   than layout maturity.

No current candidate appears to satisfy the full target experience out of the
box. The largest gaps are equal-width vertical columns, XMonad-like directional
window control, scratchpad/minimize semantics, and taffybar-compatible state.

## Current River Model

River is now a non-monolithic Wayland compositor. It does not combine the
compositor and window manager into one process; a separate window manager
implements `river-window-management-v1`.

Older River layout generators, such as `rivertile`, `river-filtile`, and
`wideriver`, are useful historical context but are not the primary path if the
goal is to use the latest River architecture.

## Candidate Ranking

### 1. kwm

Repository: <https://github.com/kewuaa/kwm>

Fit: best practical starting point.

Reasons to consider it:

- Zig.
- Most popular candidate found.
- Very active as of late April 2026.
- Implements `river-window-management-v1`.
- Dynamic tiling with `tile`, `grid`, `monocle`, `deck`, `scroller`, and
  floating layouts.
- Has tags, shift-tags, regex window rules, modes, runtime configuration reload,
  and several window states.
- Uses a runtime `config.zon` file with partial overrides and live reload; it is
  DWM-like, but ordinary configuration is not patch-based.

Risks:

- It is DWM-like, not XMonad-like.
- Default bindings are mostly next/previous rather than geometric WASD
  directional focus and movement.
- The closest built-in layout to equal columns is probably `grid`, but this
  needs empirical testing.
- It has its own status bar model; taffybar integration is not solved.

Use this first if the goal is to get a working River session and discover
practical gaps quickly.

### 2. orilla

Repository: <https://git.sr.ht/~hokiegeek/orilla>

Fit: best philosophical match.

Reasons to consider it:

- Rust.
- Explicitly inspired by XMonad.
- Code-first configuration model.
- Dynamic tiling with composable layouts.
- Has tag actions for switching, shifting, previous tag, next/previous tag,
  next empty tag, and shift-to-empty tag.
- Has a fullscreen/monocle-like layout.

Risks:

- Early project.
- Low adoption compared with `kwm`.
- Built-in layouts do not appear to include the exact equal-width vertical
  columns layout.
- Current exposed actions are ordered focus/swap actions, not geometric
  directional navigation.
- The implementation stores window rectangles, so directional navigation looks
  patchable, but it is not available today.
- Current layout application appears early in its multi-output support; layout
  calculation uses the first output rather than a mature per-output model.
- Output movement, scratchpads, minimization, and taffybar state would likely
  require extension work.

Use this first if the goal is to build toward the exact target experience rather
than maximize out-of-the-box coverage.

### 3. rhine

Repository: <https://codeberg.org/sivecano/rhine>

Fit: interesting integration candidate.

Reasons to consider it:

- Zig.
- Active as of late April 2026.
- Modular design.
- Has directional commands such as `moveFocus <dir>` and `moveWindow <dir>`.
- Has workspace commands including send-to-workspace and move-to-workspace.
- Provides partial Hyprland IPC compatibility, which may be useful for tooling
  and taffybar experiments.

Risks:

- Only a BSP-style layout appears to be implemented currently.
- Special workspaces and some monitor/workspace behavior are still TODO-level.
- Less popular than `kwm`.
- Equal vertical columns and tabbed/monocle behavior are not obvious fits.

Use this for a small spike if the Hyprland IPC bridge looks like the shortest
path to taffybar state.

### 4. beansprout

Repository: <https://codeberg.org/beansprout/beansprout>

Fit: viable but less aligned.

Reasons to consider it:

- Zig.
- Active as of late April 2026.
- DWM-style dynamic tiling.
- KDL configuration.

Risks:

- Primary/stack model conflicts with equal-width columns.
- Per-output tags conflict with the target shared global workspace model.
- Less popular than `kwm`.

### 5. rill

Repository: <https://codeberg.org/lzj15/rill>

Fit: low.

Reasons to consider it:

- Zig.
- Active.
- More popular than several smaller candidates.

Risks:

- It is a scrolling window manager, which does not match the target layout
  model.

### 6. notion-river

Repository: <https://github.com/Marenz/notion-river>

Fit: low despite good tab support.

Reasons to consider it:

- Rust.
- Has persistent frames and tabbed windows.
- Has IPC and cross-monitor focus/move behavior.

Risks:

- It is explicitly static tiling, not dynamic tiling.
- Persistent frames conflict with the target equal dynamic columns behavior.
- Workspaces are assigned per output.

### 7. rivulet

Repository: <https://github.com/jackiedorland/rivulet>

Fit: watchlist only.

Reasons to consider it:

- Haskell.
- Configured in a Haskell DSL.
- Conceptually closest to the desire for an XMonad-like programmable WM.

Risks:

- The README currently says not to use it and that it is not usable as a window
  manager yet.
- Many core behaviors are still unfinished.

### Historical Option: wideriver

Repository: <https://github.com/alex-courtis/wideriver>

Fit: not a latest-protocol candidate.

Reasons it is still worth knowing about:

- Inspired by DWM and XMonad.
- Has layouts closer to classic dynamic tiling expectations.
- More established than several current-protocol candidates.

Reason not to choose it now:

- It belongs to the older layout-generator model, not the latest River
  `river-window-management-v1` window-manager model.

## Requirement Fit

### Dynamic Tiling

Best candidates: `kwm`, `orilla`, `beansprout`.

`kwm` has the broadest ready-made layout set. `orilla` is the better extension
target if writing a custom layout is acceptable.

### Equal Vertical Columns

No candidate clearly satisfies this out of the box.

Likely approaches:

- Test `kwm` grid layout first.
- Implement an `EqualColumns` layout in `orilla`.
- Avoid primary/stack defaults unless they can be configured into equal columns.

### Monocle or Fullscreen-Like Layout

Best candidates: `kwm`, `orilla`, `notion-river`.

`kwm` has monocle. `orilla` has a fullscreen/monocle-like layout. `notion-river`
has strong tabbed behavior but fails the dynamic-layout requirement.

### Directional Navigation and Window Movement

Best candidate: `rhine`.

`rhine` exposes directional focus and movement commands. `kwm` and `orilla`
appear to need either custom action work or careful testing to determine whether
geometric WASD behavior can be made exact.

### Workspace Movement

Best candidates: `orilla`, `kwm`, `rhine`.

`orilla` has explicit tag actions for last tag, next/previous tag, next empty
tag, and shift-to-empty. `kwm` has tags and shift-tags. `rhine` has explicit
send-to-workspace and move-to-workspace commands.

### Scratchpads and Minimization

No candidate clearly solves this in the target form.

Likely implementation requires reserved hidden/special state in the window
manager, not just shell scripts. River's protocol has hide/show primitives, so a
native WM implementation has enough substrate, but each candidate needs review or
extension.

### Overview and Discovery

No candidate clearly provides a Hyprexpo-like overview.

Practical first implementation is probably a rofi/launcher workflow built from
window state. `lswt` may be useful as a compositor-neutral Wayland toplevel
source, but accurate workspace positioning is likely WM-specific.

### Taffybar State

Best candidate to investigate: `rhine`.

The partial Hyprland IPC compatibility is the only candidate feature that looks
directly relevant to existing Hyprland-oriented tooling. Otherwise, taffybar
will need a River/current-WM backend or a normalization daemon.

Required state includes:

- workspaces/tags
- focused output
- active workspace per output
- workspace history per output
- windows per workspace
- app-id/class, title, active state, urgency if available
- minimized and scratchpad filtering
- approximate window position

## Proposed Prototype Plan

1. Package or run `kwm` and `orilla` locally without making either a default
   login session.
2. Start with `kwm` to measure out-of-the-box usability: tags, previous tag,
   grid, monocle, send-to-tag, shift-to-empty, and output movement.
3. In parallel or immediately after, sketch an `orilla` configuration with an
   `EqualColumns` layout and the workspace actions needed by the spec.
4. Do a small `rhine` spike only for Hyprland IPC and taffybar feasibility.
5. Choose between `kwm` and `orilla` after testing layout fidelity and state
   export.

Expected decision:

- Choose `kwm` if it gets close enough with configuration and light scripting.
- Choose `orilla` if exact XMonad-like behavior matters enough to write Rust.
- Do not choose `rivulet` until it becomes usable.
- Do not choose `notion-river` unless the dynamic-layout requirement changes.

## Orilla Deep Dive

Commit checked: `bd77afb`, "adds support for finding next empty tag".

### What Orilla Already Has

- Rust code-first configuration.
- A small action model for keybindings.
- Dynamic layout abstraction through the `Layout` trait.
- Built-in `Tall` and `Full` layouts.
- Per-tag layout selection hooks.
- Tags with switch, toggle, shift, last, next, previous, next empty, and
  shift-to-empty actions.
- Window tracking for internal IDs, app-id, title, geometry, focus, tags,
  hidden state, and closed state.
- River hide/show calls for non-visible windows.
- A simple JSON IPC stream over a Unix socket containing tags, focused tag,
  windows, app-id/title, focused window state, and current layout.
- Passing test suite at the checked commit.

### Directional Navigation

Orilla does not currently provide geometric directional navigation.

Existing window actions are:

- close focused window
- focus primary
- focus next
- focus previous
- promote focused window
- swap next
- swap previous

This means `Super+WASD` cannot be mapped directly to "focus the closest window
in that direction" today.

The good news is that Orilla stores enough geometry to implement it. Each window
has `x`, `y`, `width`, and `height`, and layout application updates those fields.
A plausible patch would add:

- `Direction::{Up, Down, Left, Right}`
- `WindowAction::FocusDirection(Direction)`
- `WindowAction::SwapDirection(Direction)`
- geometry helpers on `WindowState`
- keybinding constructors such as `action::focus_direction(Direction::Left)`

The selection heuristic should match XMonad-style behavior: compare candidate
window rectangles against the focused window rectangle, filter to the requested
direction, prefer overlapping windows on the perpendicular axis, then minimize
distance.

### Equal Columns Layout

Orilla does not currently include the desired equal-width vertical columns
layout.

This is straightforward to implement as a new `Layout`: divide the output width
by the number of visible windows, give each visible window full output height,
and distribute remainder pixels across the first columns.

This is likely easier in Orilla than in most alternatives because layouts are
plain Rust implementations of a small trait.

### Fullscreen and Monocle

Orilla has a `Full` layout. It places every visible window at the full output
rectangle and sorts the focused window last so it renders on top.

This covers monocle-style behavior, but it is not tabbed. Tab UI would need a
separate layer-shell surface, IPC consumer, or native extension.

Directional focus could still work in `Full` if implemented against window order
as a fallback when all rectangles overlap.

### Workspaces and Tags

Orilla's tag model is useful for the target workspace behavior:

- switch to specific tag
- move focused window to specific tag
- toggle visible tag
- last tagset
- next/previous tag
- next empty tag
- move focused window to next empty tag

Gaps:

- "move window to workspace and follow" is not a first-class action, but can be
  added by combining shift and switch semantics.
- Last workspace is global to the tag manager, not per monitor.
- There is no monitor-specific workspace history yet.

### Monitors and Outputs

This is the largest architectural gap after directional navigation.

Orilla tracks River outputs, but windows do not currently carry an output
assignment. Layout application uses the first output from the output map and
lays out the global visible window set there. That is enough for early/single
monitor use, but not enough for the target multi-monitor experience.

To meet the spec, Orilla likely needs:

- active output tracking
- window-to-output assignment
- per-output visible tag/history model, or a shared tag model with explicit
  output occupancy
- layout calculation per output
- focus-output and send-window-to-output actions
- useful focus behavior after moving a window between outputs

### Scratchpads and Minimization

Orilla has the basic substrate but not the feature.

The substrate:

- windows have tags
- windows have hidden state
- the WM can call River hide/show
- app-id/title are tracked

Missing:

- named scratchpad rules
- delayed title/app-id matching policy
- restore behavior
- minimized-window list/order
- keybinding actions for hide/restore
- IPC fields distinguishing normal, scratchpad, and minimized windows

### Taffybar State

Orilla's existing IPC is promising but too small.

Current IPC exports tags, focused tag, current layout, and each tag's windows
with app-id/title/focused. It does not yet export output identity, active output,
window geometry, hidden/minimized/scratchpad classification, urgency, or
per-monitor history.

This is still a better starting point than having no state export, because it is
already JSON over a Unix socket and sits inside the WM where the necessary state
should live.

### Bottom Line

Orilla has the right shape if we are willing to write WM code. It does not have
the exact daily-driver behavior today.

The likely minimum patch set before serious use is:

1. Equal-columns layout.
2. Geometric directional focus and swap.
3. Move-focused-window-to-tag-and-follow.
4. Multi-output model.
5. IPC expansion for taffybar and window menus.
6. Scratchpad/minimize state.
