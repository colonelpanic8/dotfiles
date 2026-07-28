# T3 Code personal assembly — shared model for the skills

The installed T3 Code is upstream `main` plus ~35 carried topic branches,
assembled by ordered 3-way merges into a throwaway integration branch that
NixOS pins by revision. `assembly.toml` in the dedicated `t3code-assembly`
repository is the intent; `t3code/assembled` is the artifact; the lock records
what was built. The repository pins its upstream base through a `t3code/`
submodule.

**The authoritative reference for all build procedure is `BUILDING.md` in
`t3code-assembly`** — it is versioned with the tooling it describes. This guide
holds only what the skills share: locations, operation boundaries, and
machine-local safety invariants. Do not duplicate procedure from BUILDING.md
into skills or this guide.

Read the T3 Code repo-root `AGENTS.md` and `CONTRIBUTING.md` before operating
in the checkout.

## Fixed locations

| Item | Location |
| --- | --- |
| Assembly repository | `/srv/dotfiles/dotfiles/agents/project-links/t3code-assembly` |
| Assembly source submodule | `t3code/` inside the assembly repository |
| T3 Code development checkout | `/srv/dotfiles/dotfiles/agents/project-links/t3code` |
| Remotes | `origin` = pingdotgg/t3code, `fork` = colonelpanic8/t3code |
| Assembly repository remote | `colonelpanic8/t3code-assembly` |
| Authoritative build guide | `BUILDING.md` in the assembly repository |
| Manifests, locks, patches, tools | Assembly repository root |
| Integration branch (artifact) | `fork/t3code/assembled` + dated tags |
| Flake input | `t3code-integration` in `/srv/dotfiles/nixos/flake.nix` |
| Dotfiles consumer | `overlays.client` applied in `/srv/dotfiles/nixos/nix.nix` |

The integration branch is a build artifact: never commit to it, base work on
it, or merge it back. The flake must pin a revision, never the branch.

## Four operations, four skills

1. **Inventory** (`$inventory-t3code-assembly`) — read-only. Reconcile live
   PRs and branches against manifests and locks; report. Never mutates.
2. **Branch refresh** (`$refresh-t3code-pr-branches`) — maintain writable
   topic branches: review fixes, rebase onto live `origin/main`, push. Never
   touches manifests, the integration branch, or Nix.
3. **Assembly rebuild** (`$rebuild-t3code-assembly`) — change integration intent or
   output: edit manifests, rebuild, verify, publish, repin, activate, commit.
4. **Submit change** (`$submit-t3code-change`) — implement a new focused PR,
   then integrate it via the rebuild procedure.

Invoke only the operation the user requested; a read-only inventory may
recommend a mutation workflow but must not silently perform it.

## Topic classes

- **Maintained PR branches** — `colonelpanic8` fork branches; writable,
  rebasable.
- **Local topics** — fork branches with no PR (e.g. `t3code/local/nix-flake`);
  writable, rebased like PR branches.
- **Branch-linked topics** — manifest entries with a `branch` but no open PR;
  the branch is authoritative even if a former PR was closed.
- **External topics** — `kind = "external"`, owned by others; read-only,
  merged from the PR head.
- **Groups** — sub-manifests pinned as one main-manifest entry; rebuilt first.
- **Epilogues** — minimal patches whose meaning depends on the assembled tree.

Do not conflate open PRs, maintained branches, and carried topics: closed PRs
can stay intentionally carried, and merged PRs stay carried until the selected
upstream revision contains them. Prove containment with ancestry or diff
evidence, never from titles. Treat `ABSORBED`/`EMPTY` rebuild flags as drop
candidates, not automatic deletions.

## Safety invariants

- Work only in the primary `/srv/dotfiles` checkout; never create a dotfiles
  worktree, and never place a nested T3 Code worktree under `/srv/dotfiles`.
- Preserve dirty assembly state, T3 Code worktrees, and unrelated dotfiles
  changes or index entries.
- Fetch and record live `origin/main` before each mutation phase; if it moves,
  reassess affected rebases and rebuilds.
- Serialize force-pushes to a branch, manifest edits, final builds, and
  commits.
- Run NixOS activation only with `just switch` from `/srv/dotfiles/nixos`.
- Commit coupled files (manifest + lock + submodule pin; flake pin + flake.lock)
  with explicit paths so unrelated staged files cannot be swept in.
