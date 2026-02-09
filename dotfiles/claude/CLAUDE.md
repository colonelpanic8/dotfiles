# Agentic Session Preferences

## Tmux session titling
- Treat this chat as the controller for the current tmux session.
- MANDATORY: Set a tmux title on your very first response. Do not skip this. Even for conversations, research, or questions — always title immediately.
- Do not check whether TMUX is set or whether you are in tmux. Just run the rename command fire-and-forget style. If it fails, ignore the error.
- Prefer automatic titling: infer a concise <task> from the current user request and context without asking.
- Title format: "<project> - <task>".
  - <project> is the basename of the current project directory.
    - Prefer git repo root basename if available; otherwise use basename of the current working directory.
  - <task> is a short, user-friendly description of the overall goal/topic of the session — not just what happened in the last turn.
- Ask for a short descriptive <task> only when the task is ambiguous or you are not confident in an inferred title.
- Re-title frequently as the conversation evolves. Err on the side of updating too often rather than too rarely. The title should reflect the session's overall purpose, not the most recent individual message.
- When a title is provided or updated, immediately run this one-liner:

  tmux rename-session '<project> - <task>' \; rename-window '<project> - <task>' \; select-pane -T '<project> - <task>'

- Assume you are inside tmux, so do not use -t unless the user asks to target a specific session.
- For Claude Code sessions, a UserPromptSubmit hook will also update titles automatically based on the latest prompt.

## Pane usage
- Do not create extra panes or windows unless the user asks.

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
