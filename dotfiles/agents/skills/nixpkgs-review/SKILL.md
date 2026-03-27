---
name: nixpkgs-review
description: Review or prepare nixpkgs package changes and PRs using a checklist distilled from review feedback on Ivan Malison's own NixOS/nixpkgs pull requests. Use when working in nixpkgs on package inits, updates, packaging fixes, or before opening or reviewing a nixpkgs PR.
---

# Nixpkgs Review

Use this skill when the task is specifically about reviewing or tightening a change in `NixOS/nixpkgs`.

The goal is not generic style review. The goal is to catch the kinds of issues that repeatedly came up in real nixpkgs feedback on Ivan's PRs: derivation structure, builder choice, metadata, PR hygiene, and JS packaging details.

## Workflow

1. Read the scope first.
   Open the changed `package.nix` files, related metadata, and the PR title/body if there is one.

2. Run the historical checklist below.
   Bias toward concrete review findings and actionable edits, not abstract style commentary.

3. Validate the package path.
   Use the narrowest reasonable validation for the task: targeted build, package eval, or `nixpkgs-review` when appropriate.

4. If you are writing a review:
   Lead with findings ordered by severity, include file references, and tie each point to a nixpkgs expectation.

5. If you are preparing a PR:
   Fix the checklist items before opening it, then confirm title/body/commit hygiene.

## Historical Checklist

### Derivation structure

- Prefer `finalAttrs` over `rec` for derivations and nested derivations when self-references matter.
- Prefer `tag = "v${...}"` over `rev` when fetching a tagged upstream release.
- Check whether `strictDeps = true;` should be enabled.
- Use the narrowest builder/stdenv that matches the package. If no compiler is needed, consider `stdenvNoCC`.
- Put source modifications in `postPatch` or another appropriate hook, not inside `buildPhase`.
- Prefer `makeBinaryWrapper` over `makeWrapper` when a compiled wrapper is sufficient.
- Keep wrappers aligned with `meta.mainProgram` so overrides remain clean.
- Avoid `with lib;` in package expressions; prefer explicit `lib.*` references.

### Metadata and platform expectations

- For new packages, ensure maintainers are present and include the submitter when appropriate.
- Check whether platform restrictions are justified. Do not mark packages Linux-only or broken without evidence.
- If a package is only workable through patch accumulation and has no maintainer, call that out directly.

### JS, Bun, Electron, and wrapper-heavy packages

- Separate runtime deps from build-only deps. Large closures attract review attention.
- Remove redundant env vars and duplicated configuration if build hooks already cover them.
- Check bundled tool/runtime version alignment, especially browser/runtime pairs.
- Install completions, desktop files, or icons when upstream clearly ships them and the package already exposes the feature.
- Be careful with wrappers that hardcode env vars users may want to override.

### PR hygiene

- PR title should match nixpkgs naming and the package version.
- Keep the PR template intact unless there is a strong reason not to.
- Avoid unrelated commits in the PR branch.
- Watch for duplicate or overlapping PRs before investing in deeper review.
- If asked, squash fixup history before merge.

## Review Output

When producing a review, prefer this shape:

- Finding: what is wrong or risky.
- Why it matters in nixpkgs terms.
- Concrete fix, ideally with the exact attr/hook/builder to use.

If there are no findings, say so explicitly and mention remaining validation gaps.

## References

- Read [references/review-patterns.md](references/review-patterns.md) for the curated list of recurring review themes and concrete PR examples.
- Run `scripts/mine_pr_feedback.py --repo NixOS/nixpkgs --author colonelpanic8 --limit 20 --format markdown` to refresh the source material from newer PRs.
