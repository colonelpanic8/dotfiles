# Agentic Session Preferences (taffybar)

## Ad hoc running
- Prefer `just run` (foreground) and `just restart` (background) when running taffybar during iterative work.
- If running manually, prefer `direnv exec . cabal run` (or `nix develop -c cabal run` if direnv is not active) so you get the right build inputs and environment.

## Multiplexer session titling
- If the `TMUX` or `ZELLIJ` environment variable is set, treat this chat as the controller for the current tmux or zellij session.
- Use `set_multiplexer_title '<project> - <task>'` to update the title. The command detects tmux vs. zellij internally, prefers tmux when both are present, and no-ops outside a multiplexer.
- Maintain a session/window/pane title that describes the durable purpose of the overall exchange.
- Prefer automatic titling: infer a concise <task> from the current user request and the existing chat context without asking.
- Choose holistic titles over granular turn summaries. The title should answer "what has this chat been for?" rather than describe the latest command, substep, clarification, or follow-up message.
- Preserve the existing <task> when the new user turn is a continuation, status check, refinement, or implementation detail within the same broader objective.
- Title format: "<project> - <task>".
  - <project> is the basename of the current project directory.
    - Prefer git repo root basename if available; otherwise use basename of the current working directory.
  - <task> is a short, user-friendly description of what we are doing.
- Ask for a short descriptive <task> only when the task is ambiguous or you are not confident in an inferred title.
- When the broader objective changes substantially, update the <task> automatically if clear; otherwise ask for an updated <task>.
- When a title is provided or updated, immediately run `set_multiplexer_title '<project> - <task>'`; do not call raw tmux or zellij rename commands unless debugging the helper itself.

## Pane usage
- Do not create extra panes or windows unless the user asks.

## NixOS workflow
- This system is managed with a Nix flake at `/etc/nixos` (`/srv/dotfiles/nixos`).
- Use `just switch` from that directory for rebuilds instead of plain `nixos-rebuild`.
- Host configs live under `machines/`; choose the appropriate host when needed.

## Skills
A skill is a set of local instructions to follow that is stored in a `SKILL.md` file.
