# Taffybar constellation

## Scope
- Use this guide for requests involving taffybar itself or local taffybar configuration.

## Related packages/projects (trigger list)
- If any of these names are mentioned, open this guide for context.
- `taffybar`: top-level desktop bar library/app.
- `imalison-taffybar`: personal taffybar configuration package/repo.
- `gtk-sni-tray`: StatusNotifier tray integration for taffybar.
- `gtk-strut`: X11/WM strut handling used by taffybar ecosystem.
- `status-notifier-item`: StatusNotifier protocol/types library.
- `dbus-menu`: DBus menu protocol support used by tray integrations.
- `dbus-hslogger`: DBus logging helper used in ecosystem packages.

## Symlink targets
- `./project-links/taffybar-main` -> main taffybar repo.
- `./project-links/taffybar-config` -> local taffybar config root.

## Discovery hints
- Start with `~/.config/taffybar`.
- Common layout is:
  - config root at `~/.config/taffybar`
  - main repo at `~/.config/taffybar/taffybar`
- Other taffybar-related repos may exist elsewhere; find them from docs in the main repo.

## Read-first docs
- `./project-links/taffybar-main/README.md`
- `./project-links/taffybar-config/README.md` (if present)
- `./project-links/taffybar-config/AGENTS.md` (if present)
