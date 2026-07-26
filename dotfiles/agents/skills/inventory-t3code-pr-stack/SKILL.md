---
name: inventory-t3code-pr-stack
description: "Read-only inventory of Ivan's T3 Code pull requests, fork branches, carried manifest topics, and published integration lock. Use when asked whether new T3 Code branches or PRs have been incorporated, what the stack currently carries, whether topics are missing or redundant, or for an evidence-backed stack status report without changing branches, manifests, Nix pins, or the installed build."
---

# Inventory the T3 Code PR stack

Read `/srv/dotfiles/dotfiles/agents/project-guides/t3code-pr-stack.md`
completely before acting. This skill is strictly read-only.

## Inventory

1. Read the T3 Code repo-root `AGENTS.md` and `CONTRIBUTING.md`.
2. Verify `gh auth status`.
3. Fetch `origin/main` and `fork`, then record the live main OID. Fetching is
   allowed; do not check out, rebase, merge, edit, or push.
4. Read `stack/BUILDING.md`, both manifests, and both locks from
   `fork/t3code/stack-tooling`.
5. Query all relevant `colonelpanic8` PRs, including open, closed, and merged
   states. Capture number, title, state, review decision, base/head OIDs, head
   branch, creation/update times, writability, and checks.
6. Build three separate inventories:
   - maintained writable PR and local branches;
   - every carried manifest topic, including external and branch-linked topics;
   - owned PRs created since the main lock's `generated` timestamp.
7. Reconcile exact heads and pins. For each apparently missing PR, prove direct
   carriage, upstream absorption, exact ancestry, or behavior-level subsumption.
   Do not treat a matching title or overlapping files as proof.
8. Identify manifest pins that moved, writable branches not based on live main,
   `ABSORBED`/`EMPTY` candidates from the lock, and duplicated cumulative
   branches.

## Stop boundary

Do not modify any branch, PR, manifest, lock, flake file, running installation,
or remote state. If changes are warranted, recommend the precise next skill:

- `$refresh-t3code-pr-branches` for review fixes or rebases.
- `$rebuild-t3code-stack` for manifest admission/removal, assembly, publication,
  Nix repinning, build, or activation.

## Report

Report the upstream OID, lock generation time and integration OID, newly
discovered PRs, per-topic carriage evidence, moved heads, missing or redundant
topics, and unrelated dirty state left untouched. Clearly distinguish facts
from recommendations.
