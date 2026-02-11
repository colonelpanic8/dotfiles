# Taffybar SNI Menu CSS Debug Notes

This documents the root cause and debugging workflow for the "SNI tray submenu text is unreadable" and "menus become transparent" issues.

## Background

Taffybar renders StatusNotifierItem (SNI) menus using GTK menus created from the application's DBusMenu (`StatusNotifierItem.Menu`).

Key GTK behavior that matters here:

- SNI menus are shown as separate popup windows (GTK creates `window.popup` / menu surfaces).
- Those popup windows can *inherit style context* from the widget chain they are attached to (`Gtk.menuAttachToWidget`).
- This means bar CSS can "leak" into menu windows even though the menus are not children of the bar in the widget hierarchy.

## What We Observed

1. **Blanket descendant rules leak into menus**
   - Rules like `.taffy-window .taffy-box * { color: ... }` can override the theme foreground color for menu text.
   - Rules like `... * { background-color: transparent; }` can make menu surfaces transparent (especially submenus).

2. **The main culprit was the "pill solid" background reset**
   - The bar used a broad "reset" to make widget pills look solid by clearing backgrounds on many descendants.
   - Even with attempted exclusions like `:not(menu):not(menuitem):not(popover):not(window)`, the selector was still too broad in practice and resulted in transparent menus.

3. **Wayland popup restrictions complicate programmatic reproduction**
   - Under Wayland, GTK may refuse to show real `GtkMenu` popups without a triggering input event (serial), producing warnings like:
     - `no trigger event for menu popup`
   - This makes "programmatically popup a real menu" unreliable without simulating input.

## How We Isolated It

We created a minimal stylesheet and reintroduced rules incrementally:

- `scratch.css` started essentially empty, then we added bar/pill styling only (no resets).
- Menus were fine.
- Adding the broad background reset reproduced the problem immediately.
- Replacing the broad reset with a narrow, targeted reset fixed the problem.

## Working Fix

In `scratch.css` the "pill solid" reset was replaced with a *safe reset* that does not use broad `*` descendant selectors:

- Only clear backgrounds on common bar widgets: `label`, `image`, `button` under `.outer-pad/.inner-pad/.contents`.
- Avoid `... * { ... }` rules in the bar where possible.

This preserved pill visuals while keeping SNI menus theme-driven and opaque.

## Practical Guidance (CSS)

Avoid in bar CSS:

- Broad descendants: `.something * { ... }`
- Broad transparency resets: `background-color: transparent` applied to large subtrees

Prefer:

- Targeted selectors (specific widget types/classes)
- Rules scoped to the bar window/container classes (`.taffy-box`, `.outer-pad`, `.inner-pad`) without `*`
- If you must use descendants, be extremely conservative and re-test SNI menus after changes

## Debug/Automation Helpers

This repo includes a DBus debug server (running inside taffybar) and scripts to help automate style debugging:

- DBus name: `taffybar.debug`
- Object path: `/taffybar/debug`
- Interface: `taffybar.debug`

Useful scripts / just targets:

- `just restart-scratch` runs taffybar with `TAFFYBAR_CSS_PATHS=scratch.css`
- `just restart-menu-debug` runs with `TAFFYBAR_CSS_PATHS=menu-debug.css`
- `just popup-sni-menu` attempts to popup menus (may be blocked on Wayland)
- `scripts/taffybar-screenshot-focused-monitor` takes a monitor screenshot with `grim`

Notes:

- Programmatic menu popups are not always possible on Wayland without a trigger event.
- If needed, prefer "preview window" or input simulation for fully automated capture.

## Next Steps

- Migrate the "safe reset" approach back into `taffybar.css` (or replace `taffybar.css` with the final `scratch.css` contents once complete).
- Keep `scratch.css` as a known-good bisect harness for future menu/theme regressions.

