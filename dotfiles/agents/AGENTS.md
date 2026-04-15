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

## Available Tools

### Chrome DevTools MCP
A browser automation MCP is available for interacting with web pages. Use it to:
- Navigate to websites and fill out forms
- Take screenshots and snapshots of pages
- Click elements, type text, and interact with web UIs
- Read page content and extract information
- Automate multi-step web workflows (booking, purchasing, form submission, etc.)

### Google Workspace CLI (`gws`)
The local `gws` CLI is available for Google Workspace operations. Use it to:
- Search, read, and send Gmail messages
- Manage Gmail labels and filters
- Download attachments and inspect message payloads
- Access Drive, Calendar, Docs, Sheets, and other Google Workspace APIs

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
- For web tasks, prefer using the Chrome DevTools MCP to automate interactions directly.
- For email tasks, prefer using `gws gmail` over navigating to Gmail in the browser.
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
