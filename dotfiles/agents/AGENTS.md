# Agentic Session Preferences

## Delegating coding work to subagents
- When the primary model is Fable 5, strongly prefer delegating implementation work to subagents (via the Agent tool). Fable should usually act as orchestrator: planning, delegating, reviewing, and integrating. It may still work directly when delegation would add disproportionate overhead, when the task cannot be usefully separated, or when the implementation itself is hard enough to warrant it.
- Prefer Sol subagents for delegated implementation work. Sol is more steerable and better at running a well-specified task to completion, which is exactly what a subagent needs to do — the orchestrator has already discharged the ambiguity. Use Opus 5 subagents when the delegated piece still carries design weight, and for review passes.
- Drop to Luna subagents when the delegated task is purely mechanical and contains no remaining decision. Fanning out many small Luna agents is often the right shape for repetitive edits.
- When the primary model is Sol or Opus 5, subagent delegation is optional rather than required. Both are workhorses and working directly is frequently correct. Use judgment: delegate when work is meaningfully parallelizable, independently scoped, or benefits from a separate implementation/review pass; work directly when that is simpler and more efficient.
- The primary agent remains responsible for reviewing and integrating delegated work.
- Whenever you pick a model tier for an agent, record a one-line justification for that choice (in your reasoning/CoT or a brief note in the delegating message) so the decision is auditable. Tie the justification to what the agent must actually decide at execution time, not just the topic's importance — a task specified tightly enough that the taste is already discharged doesn't need the stronger tier. If you can't articulate why the cheaper tier is insufficient, default to it.
- These guidelines apply to writing, editing, and refactoring code. Non-coding work (reading, searching, planning, running commands, answering questions) does not need to be delegated.

## Paseo-hosted sessions

- If `PASEO_AGENT_ID` is set, read `/srv/dotfiles/dotfiles/agents/PASEO.md` completely and follow it. If it is unset, do not load that file.

## Code verification
- Run the relevant formatting, lint, and type checks for changed code whenever the project provides them. Prefer the narrowest affected package or file scope, but do not routinely omit these static checks merely because a change is small or mechanical.
- Choose local tests in proportion to the change and their cost. Behavioral changes, bug fixes, risky refactors, and new edge cases should get focused tests that exercise the changed behavior.
- For small mechanical changes, it is acceptable to defer test execution to CI when the applicable tests are slow and a focused local test would add little signal. Run tests locally when they are reasonably fast, and report any intentional deferral clearly.
- Do not run a full repository test suite by default when focused tests or CI provide sufficient coverage, unless project-specific instructions or the user require it.

## Model and effort selection

Treat model selection and effort level as separate decisions. The following scores are subjective routing scores from 1 to 10; for cost, 10 means most expensive.

| Model | Intelligence / judgment | Design sense | Cost | Primary role |
| --- | ---: | ---: | ---: | --- |
| Fable 5 | 10 | 10 | 10 | Best planner; also the right call for genuinely tough implementation, used judiciously |
| GPT-5.6 Sol | 9.5 | 8.5 | 4 | Workhorse. Most steerable, best at grinding a goal to completion |
| Opus 5 | 9.5 | 9.5 | 7 | Workhorse. Stronger design sense; preferred where taste is entangled with the code |
| GPT-5.6 Luna | 7.5 | 5 | 1 | Mechanical and lightweight work where the task is fully specified; excellent value |

- Sol, Opus 5, and Fable 5 are the models for work that requires judgment. Luna is permitted only for the mechanical cases below. Terra, Sonnet (including Sonnet 5), GPT Mini, and Haiku should not be used, including for subagents.
- Sol and Opus 5 are the two workhorses and both are full implementation models. They are near peers in raw capability, so choose on working style rather than intelligence, with a slight general preference for Sol.
- Sol is more steerable and better at grinding down on a stated goal without stopping to ask. Default to it at high effort for implementation, debugging, and long-running agentic work, especially anything that should run to completion unattended.
- Prefer Opus 5 for implementation where design and code are entangled — UI work, API surfaces, naming-heavy refactors, anything where the right structure is discovered while writing it. Also prefer it when the value is judgment about work rather than execution of it: code review, investigation and explanation, design critique, UX writing, and independent second opinions. Its tendency to surface questions is a liability on a long unattended grind and an asset on an underspecified task.
- Use Luna for mechanical and lightweight work where the answer is already determined and the agent only has to carry it out: applying a known edit across many files, renames, formatting and lint fixes, boilerplate, extracting or reformatting data, running commands and reporting output, and simple search or file-location tasks. It is a strong value model and there is no reason to spend Sol on this tier of work.
- The test for Luna is whether the task still contains a decision. If a subagent would have to choose between reasonable alternatives, judge whether something is correct, or design anything, use Sol instead. Specifying a task tightly enough for Luna is often the cheaper move — the orchestrator makes the decisions once, then hands off pure execution.
- Do not use Luna for code review, correctness-critical changes, debugging, or anything whose failure would be quiet. Verify Luna's output rather than assuming it; the savings are only real if the work does not have to be redone.
- Use extra-high effort for unusually difficult but reasonably well-specified engineering, including complex debugging, algorithms, migrations, concurrency, multi-system changes, and high-stakes correctness review.
- Medium effort is the minimum permitted configuration for Sol, Opus 5, and Fable 5. Use it for bounded work, exploration, summaries, verification, and simple coordination. Luna may run at low effort, since the tasks it should be given do not benefit from deeper reasoning.
- Fable 5 is the best planner. Prefer it for design planning, product architecture, UI/UX direction, API design, and work where ambiguity, intuition, or taste materially affects the result.
- Fable 5 is also the right call for genuinely tough implementation, not only planning — but use it judiciously rather than reaching for it whenever a task looks hard. Sol or Opus 5 at extra-high effort covers most difficult-but-clear work at a fraction of the cost.
- Use Fable 5 at medium effort for ordinary design planning and at high effort for truly meaty, ambiguous, high-stakes, or long-horizon work.
- Do not automatically use low or max effort. Medium is the floor, high is the general default, and extra high is reserved for tasks where deeper reasoning is likely to affect correctness.
- Difficulty alone does not settle the choice. Prefer Sol or Opus 5 at extra-high effort when execution is difficult but the desired outcome is clear; prefer Fable 5 when determining the right outcome requires judgment or taste, or when the difficulty is deep enough that the cost is worth it.
- For substantive plans, use an independent subagent as a critic when the risk justifies it; prefer a Paseo-managed subagent when one is available.

### Provider preference and plan capacity
- The user may state a preference for one provider's models — Claude (Fable 5, Opus 5) or Codex (Sol, Luna) — usually because there is more remaining plan capacity on that side. Treat such a preference as a routing constraint that outranks the default preferences above, and keep honoring it for the rest of the session rather than drifting back after a few tasks.
- Honor it within the quality floor, not below it. Shifting implementation from Sol to Opus 5 or the reverse is fine, since they are peers. Dropping to a banned tier to stay on the preferred provider is not — if the preferred side has nothing appropriate for the task, say so and use the other provider rather than silently downgrading.
- Fable 5 remains reserved for the cases that warrant it. A stated preference for Claude is not license to route ordinary implementation to Fable 5; prefer Opus 5.
- When capacity is the reason, prefer shifting the cheap high-volume work first and keep expensive models for decisions that materially benefit from them.
- If no preference has been stated and it would materially change how a large piece of work is routed, it is reasonable to ask which side has capacity before starting.

### Context window variants
- The `[1m]` variants (Opus 5 1M, Fable 5 1M, Sonnet 5 1M) trade cost for a 1M-token context window. The plain 200k variants are the default.
- Reach for a 1M variant only when the task genuinely needs the window: whole-repository sweeps, long-horizon sessions expected to run past a compaction, or analysis over a large corpus that cannot be usefully chunked.
- Do not pick a 1M variant as insurance against a context that has not proven too small. Subagents are usually the better answer to "too much to read" — they read broadly and return conclusions.

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
- Exception: never create, enter, or use a worktree for the dotfiles repository
  at `/srv/dotfiles`, and never place nested-repository worktrees beneath it.
  Work only in the primary `/srv/dotfiles` checkout. This restriction also
  applies to Codex/Claude helpers that create worktrees automatically.

## Git branches
- Work directly on the repository's default branch in the primary checkout unless the user explicitly asks for a feature branch or worktree.
- Do not create or switch branches as routine task setup, and do not leave requested work only on a side branch.
- Before any explicitly requested branch switch, inspect the worktree and preserve all existing changes without disrupting branches attached to other worktrees.

## Generated branches (fork-fold / fork-assembler workflows)
- Several repositories carry local work as topic branches that a tool folds into a single integration branch — commonly named `assembled`. Recognize the pattern from commit subjects like `fork-fold: merge <branch>`, an assembly manifest, or a branch whose history is a chain of merges of `fold/*`-style branches onto an upstream base.
- **Never hand-commit to a generated branch.** It is compiled output: the next regeneration rebuilds it from the topic branches, and anything committed directly onto it is silently discarded. This holds even when the branch is currently checked out, is the only place the code builds, or the change is a one-line fix.
- Land every change on the topic branch that owns that code, then regenerate the integration branch. If no topic branch owns it yet, ask which one should — do not default to the generated branch.
- A detached HEAD sitting on the generated branch is the usual trap: check what branch you are on before committing in these repos, and confirm the target branch is a source branch rather than an output one.
- If you find work already stranded on a generated branch, flag it before doing anything else — it is one rebuild away from being lost — then port it to the proper topic branch.
- After regenerating, verify the rebuild was lossless by comparing the new integration tree against the previous head (`git diff --stat <old> <new>`, or compare `git rev-parse <old>^{tree}` with the new tree). An empty diff means nothing was dropped.
- Known instances: the paseo assembly (see `./project-guides/paseo-assembly.md`), the T3 Code assembly (see `./project-guides/t3code-assembly.md`), and the `colonelpanic8/rmk` fork used by `glove80-rmk`, whose `assembled` branch is folded from its `fold/*` branches.

## GitHub pull requests
- Default to creating pull requests as ready for review, not drafts.
- Do not add a `[codex]` prefix or any other agent/tool prefix to pull request titles.
- Create a draft pull request only when the user explicitly asks for a draft or when the remote platform requires draft status.
- If using a helper, skill, or CLI wrapper that defaults to draft PRs, override that default before creating the PR.

## NixOS workflow
- This system is managed with a Nix flake at `/srv/dotfiles/nixos`.
- Use `just switch` from that directory for rebuilds instead of plain `nixos-rebuild`.
- Never run the switch from a dotfiles worktree or override `DOTFILES_WORKTREE`
  to a temporary checkout. Home Manager's out-of-store links would remain tied
  to that path after the checkout is removed.
- Host configs live under `machines/`; choose the appropriate host when needed.
- A rebuild may restart `paseo.service`, which kills every Paseo-hosted agent and
  terminal — including you, if you are one. `just switch` detects this and re-runs
  itself detached via `safe_switch` (a tmux session outside paseo's cgroup).
  Follow or retrieve a run with `tmux -L nixos-switch attach -t switch` or
  `tail -f ~/.local/state/nixos-switch/switch.log`.

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

- When filling out forms or making purchases, pull personal info from this file
  and credentials from `pass` rather than asking the user to provide them.
- If a task requires a credential not found in `pass`, ask the user rather than guessing.

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
  - `./project-guides/paseo-assembly.md` (source: `/srv/dotfiles/dotfiles/agents/project-guides/paseo-assembly.md`) — paseo, paseo-assembly. **`assembled` is compiled output — never hand-commit to it.** Read the guide before anything involving `assembled` or "adding this to my paseo build"; it is the only place that procedure lives.
