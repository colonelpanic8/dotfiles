---
name: submit-t3code-change
description: "Implement and publish a focused change to pingdotgg/t3code, then add the resulting upstream pull request to Ivan's ordered Nix integration manifest and activate the rebuilt personal build. Use when asked to change T3 Code, open or update a T3 Code PR, carry a T3 Code change locally, or make a T3 Code feature available in the personal NixOS installation even if upstream may not merge it."
---

# Submit a T3 Code Change

Carry one change through two linked deliverables:

1. A focused, ready-for-review PR against `pingdotgg/t3code:main`.
2. A manifest entry in the personal integration stack, rebuilt and activated.

Read `/srv/dotfiles/dotfiles/agents/project-guides/t3code-pr-stack.md` first
for locations and safety invariants. The PR is the canonical standalone
implementation; the stack is the durable personal integration. Create the PR
before integrating it.

## 1. Establish clean repository state

1. Read the applicable `AGENTS.md` files in both repositories.
2. Run `git status --short --branch`, `git remote -v`, and
   `git worktree list` in T3 Code; `git status --short` in `/srv/dotfiles`.
   Preserve every unrelated change. Verify `gh auth status`.
3. Fetch `origin/main` and the relevant fork refs.
4. Work in the current T3 Code worktree when it already owns the intended
   branch and is safe; otherwise use a clean worktree or create one under the
   T3 Code repo's `.worktrees/`. Do not alter a dirty worktree to make it
   usable.
5. Base the new branch directly on current `origin/main`. Keep each PR
   independently applicable to `main`; never base it on another unmerged PR or
   on the assembled stack.

## 2. Implement a focused upstream change

Keep the PR narrow; do not mix cleanup or adjacent features merely because
upstream may decline the contribution. Follow existing T3 Code architecture
and repository instructions. Add tests that exercise behavior, not
implementation details.

For UI changes: capture clear before/after images, a short video when motion
or timing matters, and exercise the real interaction in the app, not only
unit tests.

## 3. Validate and publish the PR

Run focused tests while iterating. Before publishing:

```bash
vp check
vp run typecheck
```

Plus `vp run lint:mobile` for native mobile changes and the relevant
`vp test ...` for changed behavior. Then:

1. Review the full diff and commit it as a coherent change.
2. Push the branch to `fork`.
3. Create a ready-for-review PR against `pingdotgg/t3code:main` — no draft,
   no agent prefix in the title. Use the template headings (`What Changed`,
   `Why`, `UI Changes` when applicable, `Checklist`), include exact
   validation commands, and attach required visual evidence.
4. Verify PR number, URL, base, head branch, and full head OID with
   `gh pr view`.

Do not start the integration until the published head matches the locally
validated commit.

## 4. Integrate into the stack

Follow **`stack/BUILDING.md` on `fork/t3code/stack-tooling`**, sections
"Adding a topic" through "Landing a rebuild". Re-read `stack/stack.toml`
first — another session may have changed the stack while the PR was being
prepared. Do not claim success from a source build alone.

## 5. Report

- PR URL and head OID.
- Manifest entry added, and whether it went in the main stack or a group.
- Conflicts resolved during the rebuild.
- New integration rev and tag; tree changed or unchanged.
- Build and activation result; T3 Code and dotfiles commits.
