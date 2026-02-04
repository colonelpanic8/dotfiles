# Agentic Session Preferences

## Tmux session titling
- If the TMUX environment variable is set, treat this chat as the controller for the current tmux session.
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

## NixOS workflow
- This system is managed with a Nix flake at `~/dotfiles/nixos`.
- Use `just switch` from that directory for rebuilds instead of plain `nixos-rebuild`.
- Host configs live under `machines/`; choose the appropriate host when needed.
