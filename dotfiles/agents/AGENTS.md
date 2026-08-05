# Agentic Session Preferences

## Delegation, model, and effort selection

- Before delegating work to a subagent, choosing a model or effort level, or
  answering a question about model routing, read
  `/srv/dotfiles/dotfiles/agents/DELEGATION.md` completely and follow it. It
  covers when to delegate, which model tier to pick, effort levels, provider
  preference, and the `[1m]` context-window variants.

## Paseo-hosted sessions

- If `PASEO_AGENT_ID` is set, read `/srv/dotfiles/dotfiles/agents/PASEO.md` now
  completely and follow it. Don't output text about the fact that you're reading
  it If it is unset, do not load that file.

## Code verification
- Run whatever formatting, lint, and type checks the project provides, at the
  narrowest scope covering the change. Don't skip them because a change is small
  or mechanical.
- Test in proportion to the change: behavioral changes, bug fixes, risky
  refactors, and new edge cases get focused tests exercising the changed
  behavior. Never run the full suite by default — only when the project or the
  user requires it.
- Deferring tests to CI is fine when they're slow and a local run would add
  little signal. Run them locally when they're fast, and say so when you defer.

## Sharing dev-server / preview links over tailscale
- Start the server bound to all interfaces (e.g. vite's `--host 0.0.0.0` / a
  `dev:lan` script), not just localhost, or the Tailscale link won't connect.
  Verify reachability (`curl` the `100.x` URL) before handing it over.
- When sharing a local server or preview URL, always prefer this machine's
  Tailscale address over `127.0.0.1`/`localhost`/LAN IPs, so the link opens from
  any device on the tailnet.
- Always start each dev/preview server on a new random high port (and pin it
  with `--strictPort` so it fails loudly instead of drifting). Never reuse a
  fixed port and never kill, restart, or otherwise shut down an already-running
  server — assume other sessions/users depend on it. Leave existing servers
  alone and just stand up your own.

## Git worktrees
- For a repository at `<repo_root>`, use worktree paths like `<repo_root>/.worktrees/<task-or-branch>`.
- Create `.worktrees/` if needed before running `git worktree add`.
- Only use a non-`.worktrees/` location when the user explicitly asks for a different path.

## Generated branches (fork-assembler stacks)
- Some repositories carry local work as topic branches that fork-assembler folds
  into a single generated branch — commonly named `assembled`. **Never
  hand-commit to one.** It is compiled output; the next build discards anything
  committed directly onto it.
- Recognize the pattern before committing anywhere unfamiliar: a `manifest.toml`
  + `manifest.lock.json` pair, commit subjects like `fork-fold: merge <branch>`,
  or a branch whose history is a chain of merges onto an upstream base.
- The full model, invariants, and operations live in the assembly repository's
  own root `AGENTS.md` (generated from fork-assembler's
  `templates/maintenance/AGENTS.md`). Read it before operating on a stack; do
  not work from memory of these bullets.
- The trap is the repo that has no such `AGENTS.md`: the *fork* the assembled
  branch is pushed to, such as `colonelpanic8/rmk` (used by `glove80-rmk`).
  There the warning above is all you get.
- Known stacks: `~/Projects/paseo-assembly` (see `./project-guides/paseo-assembly.md`),
  `~/Projects/t3code-assembly` (see `./project-guides/t3code-assembly.md`),
  `~/Projects/rmk-assembly`.

## GitHub pull requests
- Default to creating pull requests as ready for review, not drafts.
- Do not add a `[codex]` prefix or any other agent/tool prefix to pull request titles.

## This machine's system configuration
- This system is a NixOS machine managed by the flake at `/srv/dotfiles`, which
  is also where these instructions live.
- Before changing anything under `/srv/dotfiles` — system or Home Manager
  config, shell functions, scripts on PATH, keybinds, or a rebuild — read
  `/srv/dotfiles/AGENTS.md` and `/srv/dotfiles/nixos/AGENTS.md`. They own the
  rebuild procedure, where each kind of file belongs, and the per-repo
  invariants. Do not work from memory of how this system is arranged.

## Ad-hoc utilities via Nix
- If you want to use a CLI utility you know about but it is not currently
  available on PATH, prefer using `nix run` / `nix shell` to get it temporarily
  rather than installing it globally.
- Use `nix run` for a single command:

  nix run nixpkgs#ripgrep -- rg -n "pattern" .

- Use `nix shell` when you need multiple tools available for a short sequence of commands:

  nix shell nixpkgs#{jq,ripgrep} --command bash -lc 'rg -n "pattern" . | head'

- If you are not sure what the package is called in nixpkgs, use:

  nix search nixpkgs <name-or-keyword>

## Personal Information

- Full Legal Name: Ivan Anthony Malison
- Email: IvanMalison@gmail.com
- Country of Citizenship: United States of America
- Birthday: August 2, 1990 (1990-08-02)
- Address: 100 Broderick St APT 401, San Francisco, CA 94117, United States
- Employer: Railbird Inc.
- GitHub: colonelpanic8
- Phone: 301-244-8534
- Primary Credit Card: Chase-Reserve

## Credentials via `pass`

Many credentials and personal details are stored in `pass` (the standard unix password manager). There are hundreds of entries covering a wide range of things, so always search before asking the user for information. Use `pass find <keyword>` to search and `pass show <entry>` to retrieve values.

Examples of what's stored:
- Personal documents - driver's license, passport number, etc.
- Credit/debit cards - card numbers, expiration, CVV for various cards
- Banking - account numbers, online banking logins
- Travel & loyalty - airline accounts, hotel programs, CLEAR, etc.
- Website logins - credentials for hundreds of services
- API keys & tokens - GitHub, various services

- The store is regularly updated with new entries. Always do a dynamic lookup with `pass find` rather than assuming what's there.
- Provide credentials to tools/config at runtime via environment variables or inline `pass` usage instead of committing them.
- Never hardcode credentials or store them in plain text files.


## Project links (local symlink index)
- Paths in this section are relative to this file's directory (`dotfiles/agents/`).
- Keep a local symlink index under `./project-links/` for projects that are frequently referenced.
- Treat these links as machine-local discovery state maintained by agents (do not commit machine-specific targets).
- Reuse existing symlinks first. If a link is missing or stale, search for the repo, then update the link with:

  ln -sfn "<absolute-path-to-repo>" "./project-links/<link-name>"

- If a project cannot be found quickly, do a targeted search (starting from likely roots) and only then widen the search.

## Project constellation guides
- Keep per-constellation context in `./project-guides/` and keep this file minimal.
- When a request involves one of these projects:
  - Open the guide first.
  - If a mentioned repo/package name matches a guide's related-project list, open that guide even if the user did not name the constellation explicitly.
  - Ensure required links exist under `./project-links/`.
  - If links are missing, run a targeted search from likely roots, then create/update the symlink.
- Guide index:
  - `./project-guides/mova-org-agenda-api.md`
  - `./project-guides/taffybar.md`
  - `./project-guides/railbird.md`
  - `./project-guides/org-emacs-packages.md`
  - `./project-guides/subtr-actor-rocket-sense-rlru.md`
  - `./project-guides/t3code-assembly.md`
  - `./project-guides/paseo-assembly.md`
