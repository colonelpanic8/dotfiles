# Subtr Actor / Rocket Sense / rlru constellation

## Scope
- Use this guide for requests involving Rocket League replay parsing, replay analytics, upload flows, or the `rlrml` projects around `subtr-actor`, `rocket-sense`, and `rlru`.
- Primary anchors are `subtr-actor` for replay-domain logic, `rocket-sense` for the hosted analytics service, and `rlru` for local replay discovery/upload and PsyNet integration.

## Related packages/projects (trigger list)
- If any of these names are mentioned, open this guide for context.
- `subtr-actor`: Rocket League replay processing core and source-of-truth replay domain model.
- `rocket-sense`: replay analytics backend and React/Vite web app built on `subtr-actor`.
- `rlru`: Rust-first Rocket League replay uploader and desktop client workspace.
- `psynet`: Psyonix PsyNet client crate inside the `rlru` workspace.

## Package inventory
- `subtr-actor` repo packages:
  - Rust crates: `subtr-actor`, `subtr-actor-tools`, `subtr-actor-bakkesmod`, `rl-replay-subtr-actor`.
  - Python package/crate: `subtr-actor-py` / `subtr_actor`.
  - npm packages: `@rlrml/subtr-actor`, `@rlrml/player`, `@rlrml/stats-player`.
- `rocket-sense` repo packages:
  - Rust crates: `rocket-sense-server`, `rocket-sense-db`, `rocket-sense-storage`.
  - Web app package: `rocket-sense-web`.
  - Vendored packages may appear under `vendor/subtr-actor`; prefer the standalone `subtr-actor` checkout for source-of-truth domain changes unless the user specifically asks about the vendored copy.
- `rlru` repo packages:
  - Rust crates: `rlru`, `psynet`, `rlru-dioxus`.
  - Apps/binaries: `rlru` CLI and `rlru-dioxus` desktop client.

## Symlink targets
- `./project-links/subtr-actor` -> primary `subtr-actor` repo.
- `./project-links/rocket-sense` -> primary `rocket-sense` repo.
- `./project-links/rlru` -> primary `rlru` repo.

## Discovery hints
- Start from `~/Projects`.
- Common local paths are:
  - `~/Projects/subtr-actor`
  - `~/Projects/rocket-sense`
  - `~/Projects/rlru`
- `rocket-sense` may vendor `subtr-actor` under `vendor/subtr-actor`; prefer the standalone `subtr-actor` checkout for source-of-truth replay-domain changes unless the user specifically asks about the vendored copy.

## Read-first docs
- `./project-links/subtr-actor/AGENTS.md`
- `./project-links/subtr-actor/README.md`
- `./project-links/rocket-sense/AGENTS.md`
- `./project-links/rocket-sense/README.md`
- `./project-links/rlru/README.md`

## Notes
- Treat `subtr-actor` as the source of truth for replay parsing, frame/state extraction, stats calculators, feature matrices, and JS/WASM replay-player data contracts.
- Treat `rocket-sense` as the service/UI layer for replay hosting, metadata, processing state, auth, storage, OpenAPI, and deployed analytics workflows.
- Treat `rlru` as the local uploader/client layer for replay discovery, account/auth state, upload destinations, Dioxus desktop UX, and the reusable `psynet` client.
- For cross-repo work, check each repo's own `AGENTS.md`, `README.md`, and `justfile` before choosing commands.
