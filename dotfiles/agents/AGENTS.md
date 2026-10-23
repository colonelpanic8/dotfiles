# Agentic Session Preferences

## Paseo
Paseo is an agentic development environment and its currently my daily driver
for running agents.

- Unless the user or task explicitly says otherwise, start every agent session
  or worker created by Paseo in the provider's full-permissions mode (Codex:
  `full-access`).
- VERY IMPORTANT: when delegating, ALWAYS spawn subagents through the Paseo MCP
  tools (e.g. `create_agent`) instead of native subagents whenever they are
  available.
- Give every Paseo-managed subagent a self-contained initial prompt. It does not
  inherit the parent's full conversation automatically.
- Before choosing a provider/model/account for a subagent, run
  `~/.lib/bin/agent-delegation`. It prints any active override and remaining
  usage for each provider.
  - An active override wins over profile notes, skill defaults, and
    cross-provider pairing advice. Only an explicit choice the user made in
    the current request beats it.
  - With no override, steer the heavy, long-running work (implementers, loop
    workers) toward the provider with the most headroom relative to its reset
    time. Avoid a provider above ~80% of its weekly window unless the task
    specifically needs it, and say so when usage changed your pick.
  - When the user states a standing preference like "use Opus for
    implementers for now", record it with `agent-delegation set [--for 3d]
    "<preference>"` (default expiry 7d) and clear it with
    `agent-delegation clear`.

## Memory
- Never use Claude Code's auto-memory directory. Record durable guidance in the
  repository it concerns (its `AGENTS.md`, `BUILDING.md`, or similar), or in
  these dotfiles for cross-project preferences.

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

## Comments
- Generally avoid superflous comments
- Avoid comments that implicitly sort of respond to a request or address aspects
  of a conversation thread that won't make sense to someone reading the code
  without that context.
- Keep comments terse when you do use them.

## Tests
- Don't add tests that just check a box or test something superflous.

## Plan documents
- Don't write or commit plan docs (design plans, implementation plans, task
  breakdowns, scratch notes) into the repository they're about unless the user
  explicitly asks for them to live in the repo.
- By default, put them under `~/plans/<repo-or-project>/`, named like
  `YYYY-MM-DD-<topic>.md`, creating the directory if needed.
- Point to the plan's path when you hand off or delegate, rather than copying it
  into the repo for a subagent to find.

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
- Release fork-assembler stacks eagerly: once a build completes, publish,
  repin, and activate it, then validate (audit, smoke, tests) and fix forward
  in a follow-up rebuild. These stacks serve only the user, who accepts the
  occasional bad release. Each stack's `BUILDING.md` spells out the order.
- fork-assembler's own source is at `~/projects/fork-assembler`
  (`colonelpanic8/fork-assembler`; link: `./project-links/fork-assembler`).
- Known stacks: `~/projects/paseo-assembly` (see `./project-guides/paseo-assembly.md`),
  `~/projects/t3code-assembly` (see `./project-guides/t3code-assembly.md`),
  `~/projects/rmk-assembly`.

## GitHub pull requests
- Default to creating pull requests as ready for review, not drafts.
- Do not add a `[codex]` prefix or any other agent/tool prefix to pull request titles.

## New projects
- Before scaffolding a new project or repo, load the `new-project` skill
  (`./skills/new-project/SKILL.md`): Nix flake + direnv + just, Rust via fenix,
  Dioxus / React Native code sharing, F-Droid auto-publishing, and example repos
  to copy from.

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
- Employer: railbird inc.
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
- Ivan's Social Security number is the first line of `pass` entry
  `social-security-number`. Retrieve it there for authorized verification instead of asking Ivan for it.
- Provide credentials to tools/config at runtime via environment variables or inline `pass` usage instead of committing them.
- Never hardcode credentials or store them in plain text files.
- Claude/Anthropic (claude.ai) and ChatGPT/Codex/OpenAI have no passwords in
  `pass`; they use "Continue with Google" (ivanmalison@gmail.com).
- A login that texts a 2FA code is not a blocker: read the code yourself (see
  "Texted 2FA codes" below).

## Texted 2FA codes: fetch them yourself

**You can read the user's text messages, so texted 2FA codes are never a
reason to stop and ask the user.** When an authorized task hits an SMS/RCS 2FA,
MFA, one-time, login, or phone-verification code, trigger it and read it
yourself, then carry on. Ask the user only if the bridge is unavailable or the
code never arrives.

- Texts to 301-244-8534 are readable on every NixOS host through the Google
  Messages Multi-Device Bridge: the `gmessages` CLI (preconfigured;
  `gmessages help`) or the read-only `gmessages` MCP server.
- When a site lets you choose where to send the code, pick text message.
- Right after triggering the code, `gmessages wait --timeout 3m` (MCP:
  `wait_for_message`) returns the next incoming message; narrow it with
  `--query` or `--conversation <sender>`, and add `--since 5m` if the code may
  already have arrived. `gmessages search --since 15m` lists recent messages.
- Read only messages relevant to the current attempt, and never print tokens or
  codes in logs or replies beyond what the task needs.
- `gmessages send` texts real people from the user's phone: only when asked.
  `--profile whatsapp` reaches the WhatsApp bridge (on jimi-hendnix only).

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

## Browser Automation

- Prefer using chrome plugins and skills for tasks that can be completed
  entirely in a browser. Use computer-use tools when the task requires broader
  desktop interaction or the Chrome plugin cannot complete it.
- Claude in Chrome gives every session its own tab group and never removes
  it. Before your final response, close every tab in your session's group
  with `tabs_close_mcp` (closing the last tab removes the group), including
  blank tabs and tabs left after a failed or abandoned attempt. Use
  `tabs_context_mcp` with `createIfEmpty: false` to find the session's tabs;
  never close the user's tabs or another agent's group. The final close
  should report that the group is empty. Do not create a group to verify
  cleanup. Leave a tab open only if the user needs to keep looking at it,
  and say which one and why. If the browser is unavailable, report that
  cleanup could not be completed.
- Only call `tabs_context_mcp` with `createIfEmpty: true` when you are about
  to use the browser. Calling it just to look creates an empty group.
