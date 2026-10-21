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
- Start from `~/projects`.
- Common local paths are:
  - `~/projects/subtr-actor`
  - `~/projects/rocket-sense`
  - `~/projects/rlru`
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

## Rocket Sense production hosting

- Public origin: `https://rocket-sense.duckdns.org`. OAuth callback URLs stay on this origin.
- `jimi-hendnix` runs the server, replay worker, and PostgreSQL 16 as rootful Podman services. Configuration is in `/srv/dotfiles/nixos/rocket-sense-jimi.nix`, imported by `nixos/machines/jimi-hendnix.nix`.
- Router reservations: `jimi-hendnix` is `192.168.0.214` (`B4-8C-9D-9F-29-63`); `railbird-sf` is `192.168.0.159` (`7C-10-C9-52-DB-AB`). Forward public TCP ports 80 and 443 to jimi's matching ports.
- nginx terminates Rocket Sense TLS locally. Other HTTPS hostnames pass through by SNI to railbird-sf, preserving Mova/org-agenda-api and the existing railbird sites. The application listens on loopback port `48763`; local TLS termination uses `45443`. Keep these internal ports private.
- DuckDNS updates and the Rocket Sense privacy page are hosted on jimi. The DuckDNS encrypted token still has the filename `nixos/secrets/duckdns-token.railbird-sf.age`.
- Persistent data: `/var/lib/rocket-sense/postgres/pgdata` and `/var/lib/rocket-sense/storage`. The `.migration-complete` marker gates startup until the initial data restore is verified. Keep that marker with the data.
- Application credentials are encrypted in `nixos/secrets/rocket-sense.env.age` and decrypted by agenix into `/run/agenix/rocket-sense-environment`. Do not print them. The Podman database network alias preserves the hostname in the existing encrypted `DATABASE_URL`.
- Check `podman-rocket-sense.service`, `podman-rocket-sense-worker.service`, `podman-rocket-sense-postgres.service`, `nginx.service`, and `duckdns.timer`. Health is `/api/v1/health`.
- For an application update, build/push the image from landed Rocket Sense `master`, resolve the registry image digest, preload that exact image on jimi, and update `image` in `rocket-sense-jimi.nix`. Apply with `just switch` from `/srv/dotfiles/nixos`. Both application services follow the pinned image; there is no Kubernetes rollout step.
- Commit and push configuration changes before restoring the hourly upgrade timer: NixOS automatic upgrades use GitHub `master`, so uncommitted local hosting changes are reverted by the next upgrade.
- The old Rocket Sense k3s Deployments are scaled to zero. `rocket-sense`'s existing `deploy-railbird-sf`/Terraform recipes target that retired deployment; do not use them to update production on jimi. The old persistent volumes remain on railbird-sf as the migration backup.
- For cross-repo work, check each repo's own `AGENTS.md`, `README.md`, and `justfile` before choosing commands.
