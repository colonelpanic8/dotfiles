# Agentic Session Preferences

## Tmux session titling
- Treat this chat as the controller for the current tmux session.
- Run the tmux rename one-liner eagerly without checking whether TMUX is set first.
- Maintain a session/window/pane title that updates when the task focus changes substantially.
- Prefer automatic titling: infer a concise <task> from the current user request and context without asking.
- Title format: "<project> - <task>".
  - <project> is the basename of the current project directory.
    - Prefer git repo root basename if available; otherwise use basename of the current working directory.
  - <task> is a short, user-friendly description of what we are doing.
- Ask for a short descriptive <task> only when the task is ambiguous or you are not confident in an inferred title.
- When the task changes substantially, update the <task> automatically if clear; otherwise ask for an updated <task>.
- When a title is provided or updated, immediately run this one-liner:

  tmux rename-session '<project> - <task>' \; rename-window '<project> - <task>' \; select-pane -T '<project> - <task>'

- Assume you are inside tmux, so do not use -t unless the user asks to target a specific session.
- For Claude Code sessions, a UserPromptSubmit hook will also update titles automatically based on the latest prompt.

## Pane usage
- Do not create extra panes or windows unless the user asks.

## Git worktrees
- Default to creating git worktrees under a project-local `.worktrees/` directory at the repository root.
- For a repository at `<repo_root>`, use worktree paths like `<repo_root>/.worktrees/<task-or-branch>`.
- Create `.worktrees/` if needed before running `git worktree add`.
- Only use a non-`.worktrees/` location when the user explicitly asks for a different path.

## NixOS workflow
- This system is managed with a Nix flake at `~/dotfiles/nixos`.
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

## Credentials
- When a credential (password, API key, token, etc.) is needed to complete a task, use the `pass` utility to retrieve it.
- The pass password store lives at `~/.password-store/`.
- Use `pass show <entry>` to retrieve a secret, or `pass find <search-term>` to locate entries.
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
