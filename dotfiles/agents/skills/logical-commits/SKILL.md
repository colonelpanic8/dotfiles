---
name: logical-commits
description: Use when the user asks to split current git changes into logical commits, clean up commit history, create atomic commits, or stage by hunk. Review the whole worktree, group related changes, and produce ordered commits where each commit is a valid state (builds/tests pass with the project validation command).
---

# Logical Commits

Turn a mixed worktree into a clean sequence of atomic commits.

## Workflow

1. Inspect the full change set before staging anything.
2. Define commit boundaries by behavior or concern, not by file count.
3. Order commits so dependencies land first (types/api/schema/helpers before consumers).
4. Stage only the exact hunks for one commit.
5. Validate that staged commit state is healthy before committing.
6. Commit with a precise message.
7. Repeat until all intended changes are committed.

## 1) Inspect First

Run:

```bash
git status --short
git diff --stat
git diff
```

If there are staged changes already, inspect both views:

```bash
git diff --staged
git diff
```

## 2) Choose Validation Command Early

Select the fastest command that proves the repo is valid for this project. Prefer project-standard commands (for example: `just test`, `npm test`, `cargo test`, `go test ./...`, `nix flake check`, targeted build commands).

If no clear command exists:

1. Infer the best available command from repo scripts/config.
2. Tell the user what command you chose and why.
3. Do not claim full validation if coverage is partial.

## 3) Plan the Commit Stack

Before committing, write a short plan:

1. Commit title
2. Files and hunks included
3. Why this is a coherent unit
4. Validation command to run

If changes are intertwined, split by hunk (`git add -p`). If hunk splitting is not enough, use `git add -e` or perform a temporary refactor so each commit remains coherent and valid.

## 4) Stage Exactly One Commit

Preferred staging flow:

```bash
git add -p <file>
git diff --staged
```

Useful corrections:

```bash
git restore --staged -p <file>   # unstage specific hunks
git reset -p <file>              # alternate unstage flow
```

Never stage unrelated edits just to make the commit pass.

## 5) Validate Before Commit

Run the chosen validation command with the current staged/working tree state.

If validation fails:

1. Fix only what belongs in this logical commit, or
2. Unstage/re-split and revise the commit boundary.

Commit only after validation passes.

## 6) Commit and Verify

Commit:

```bash
git commit -m "<type>: <logical change>"
```

Then confirm:

```bash
git show --stat --oneline -1
```

Ensure remaining unstaged changes still make sense for later commits.

## 7) Final Checks

After finishing the stack:

```bash
git log --oneline --decorate -n <count>
git status
```

Report:

1. The commit sequence created
2. Validation command(s) run per commit
3. Any residual risks (for example, partial validation only)

## Guardrails

1. Keep commits atomic and reviewable.
2. Prefer hunk staging over broad file staging when a file contains multiple concerns.
3. Preserve user changes; do not discard unrelated work.
4. Avoid destructive commands unless the user explicitly requests them.
5. If a clean logical split is impossible without deeper refactor, explain the blocker and ask for direction.
