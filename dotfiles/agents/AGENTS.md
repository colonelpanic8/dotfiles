# Agentic Session Preferences

## Delegating coding work to subagents
- When the primary model is Fable, strongly prefer delegating coding tasks to subagents (via the Agent tool). Fable should usually act as the orchestrator: planning, delegating, reviewing, and integrating. It may still work directly when delegation would add disproportionate overhead or the task cannot be usefully separated.
- For other primary models, subagent delegation is optional rather than required. Use judgment: delegate when work is meaningfully parallelizable, independently scoped, or benefits from a separate implementation/review pass; work directly when that is simpler and more efficient.
- The primary agent remains responsible for reviewing and integrating delegated work.
- Whenever you pick a model tier for an agent, record a one-line justification for that choice (in your reasoning/CoT or a brief note in the delegating message) so the decision is auditable. Tie the justification to what the agent must actually decide at execution time, not just the topic's importance — a task specified tightly enough that the taste is already discharged doesn't need the stronger tier. If you can't articulate why the cheaper tier is insufficient, default to it.
- These guidelines apply to writing, editing, and refactoring code. Non-coding work (reading, searching, planning, running commands, answering questions) does not need to be delegated.

## Model and effort selection

Treat model selection and effort level as separate decisions. The following scores are subjective routing scores from 1 to 10; for cost, 10 means most expensive.

| Model | Intelligence / judgment | Design sense | Cost | Primary role |
| --- | ---: | ---: | ---: | --- |
| Fable | 10 | 10 | 10 | Design planning, ambiguity, architecture, intuition, and the hardest work |
| GPT-5.6 Sol | 9.5 | 8.5 | 4 | Default engineering model and best intelligence-to-cost choice |
| Opus | 8.8 | 9.5 | 7 | Specialized design critique, UX writing, aesthetic review, and alternative perspectives |

- Do not select models below GPT-5.6 Sol at medium effort. Terra, Sonnet, Luna, GPT Mini, and Haiku should not be used, including for subagents.
- Default to Sol at high effort for most implementation, debugging, investigation, review, research, and agentic work.
- Use Sol at extra-high effort for unusually difficult but reasonably well-specified engineering, including complex debugging, algorithms, migrations, concurrency, multi-system changes, and high-stakes correctness review.
- Sol at medium effort is the minimum permitted configuration. Use it for bounded mechanical work, exploration, summaries, verification, and simple coordination.
- Prefer Fable for most design planning, product architecture, UI/UX direction, API design, and work where ambiguity, intuition, or taste materially affects the result.
- Use Fable at medium effort for ordinary design planning and at high effort for truly meaty, ambiguous, high-stakes, or long-horizon work.
- Use Opus selectively as a specialist rather than the default design planner. Good uses include independent design critique, visual refinement, UX writing, naming, prose, and alternative aesthetic perspectives.
- Do not automatically use low or max effort. Medium is the floor, high is the general default, and extra high is reserved for tasks where deeper reasoning is likely to affect correctness.
- Difficulty alone does not require Fable. Prefer Sol at extra-high effort when execution is difficult but the desired outcome is clear; prefer Fable when determining the right outcome requires judgment or taste.

## Cross-model delegation
- Use cross-model delegation only when the user requests it or model diversity or an independent check would be useful; it is never mandatory.
- Codex should use the `claude_delegator` agent only for the specialized Opus cases below. Claude should prefer the `codex-delegator` agent when delegating to Sol. Use `$cross-agent-delegation` or its `ask-claude` and `ask-codex` wrappers when direct invocation is simpler.
- Permit at most one cross-model handoff and never recursively delegate. Keep the child read-only and advisory by default, with only one writer per worktree.
- The parent agent owns review, verification, and integration of the child's output.
- A Sol agent must never spawn, invoke, or delegate to Fable, including through cross-model wrappers or indirect subagent chains. If a task warrants Fable, select Fable as the primary model before beginning rather than allowing Sol to escalate itself.
- Sol may delegate to other Sol agents at medium effort or higher. It may use Opus only for specialized design critique, UX writing, aesthetic review, or an independent design perspective.
- Fable may delegate bounded execution, exploration, and verification work downward to Sol.

## Sharing dev-server / preview links
- When sharing a local server or preview URL, always prefer this machine's Tailscale address over `127.0.0.1`/`localhost`/LAN IPs, so the link opens from any device on the tailnet.
- Get the address with `tailscale ip -4` (the `100.x.y.z` IP) or the MagicDNS hostname from `tailscale status`. Prefer the `100.x` IP when a server's `allowedHosts` might reject a hostname.
- Start the server bound to all interfaces (e.g. vite's `--host 0.0.0.0` / a `dev:lan` script), not just localhost, or the Tailscale link won't connect. Verify reachability (`curl` the `100.x` URL) before handing it over.
- Always start each dev/preview server on a new random high port (and pin it with `--strictPort` so it fails loudly instead of drifting). Never reuse a fixed port and never kill, restart, or otherwise shut down an already-running server — assume other sessions/users depend on it. Leave existing servers alone and just stand up your own.

## Git worktrees
- Default to creating git worktrees under a project-local `.worktrees/` directory at the repository root.
- For a repository at `<repo_root>`, use worktree paths like `<repo_root>/.worktrees/<task-or-branch>`.
- Create `.worktrees/` if needed before running `git worktree add`.
- Only use a non-`.worktrees/` location when the user explicitly asks for a different path.

## Git branches
- Work directly on the repository's default branch in the primary checkout unless the user explicitly asks for a feature branch or worktree.
- Do not create or switch branches as routine task setup, and do not leave requested work only on a side branch.
- Before any explicitly requested branch switch, inspect the worktree and preserve all existing changes without disrupting branches attached to other worktrees.

## GitHub pull requests
- Default to creating pull requests as ready for review, not drafts.
- Do not add a `[codex]` prefix or any other agent/tool prefix to pull request titles.
- Create a draft pull request only when the user explicitly asks for a draft or when the remote platform requires draft status.
- If using a helper, skill, or CLI wrapper that defaults to draft PRs, override that default before creating the PR.

## NixOS workflow
- This system is managed with a Nix flake at `/srv/dotfiles/nixos`.
- Use `just switch` from that directory for rebuilds instead of plain `nixos-rebuild`.
- Host configs live under `machines/`; choose the appropriate host when needed.

## Ad-hoc utilities via Nix
- If you want to use a CLI utility you know about but it is not currently available on PATH, prefer using `nix run` / `nix shell` to get it temporarily rather than installing it globally.
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

## Repository Overview

This is an org-mode repository containing personal task management, calendars, habits, and project tracking files. It serves as the central hub for Ivan's personal organization.

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


## Guidelines

- When filling out forms or making purchases, pull personal info from this file and credentials from `pass` rather than asking the user to provide them.
- If a task requires a credential not found in `pass`, ask the user rather than guessing.
- This repo's org files (gtd.org, calendar.org, habits.org, projects.org) contain task and scheduling data. The org-agenda-api skill/service can also be used to query agenda data programmatically.

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
