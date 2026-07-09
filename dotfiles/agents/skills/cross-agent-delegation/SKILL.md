---
name: cross-agent-delegation
description: Safely delegate between Codex and Claude through read-only CLI wrappers. Use when the user explicitly asks Codex or Claude to delegate to the other, or when a workflow explicitly calls for an independent cross-model second opinion.
---

# Cross-Agent Delegation

Use the minimum permitted native driver agent when available. Invoke the wrapper directly when that is simpler:

- Pipe a self-contained prompt to `ask-claude` or `ask-codex` on stdin.
- Reserve `--base64 '<payload>'` for native driver agents as injection-safe transport. A driver must encode the complete task itself and never embed raw delegated task text in shell source.
- Treat the returned plain text as advisory output; the wrappers are read-only and ephemeral.
- Review and verify the output in the parent agent before using it.
- Never include credentials, secrets, or unnecessary personal data in the prompt.
- Permit at most one cross-model handoff. Never ask the child to invoke either CLI or delegate again.
- A Sol caller may target Opus for specialized design work but must never target Fable. Choose Fable as the primary model before the task begins when Fable is warranted.
- Keep one writer per worktree. Have the parent make any resulting edits.

Environment variables documented in the scripts may override their conservative model, effort, timeout, turn, and budget defaults.
