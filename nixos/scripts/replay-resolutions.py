#!/usr/bin/env python3
"""Replay conflict resolutions from a previous integration build.

A previous integration branch already contains, in each `stack: merge <label>`
commit, the exact resolved content for that entry. When rebuilding with an
unchanged prefix of the manifest, those resolutions can be replayed verbatim
instead of re-derived -- the same role rerere plays, but keyed on the entry
label and sourced from a real build rather than a local cache.

Only safe while the previous build's entry ORDER matches the current one up to
this point. Past the first manifest insertion/reorder, resolutions must be
re-derived.
"""

from __future__ import annotations

import argparse
import json
import subprocess
import sys
from pathlib import Path


def git(repo: str, *args: str) -> str:
    return subprocess.run(
        ["git", "-C", repo, *args], capture_output=True, text=True
    ).stdout.strip()


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--repo", default="/home/imalison/Projects/t3code")
    parser.add_argument(
        "--from-build",
        required=True,
        help="ref of a previous integration branch to source resolutions from",
    )
    parser.add_argument("--label", required=True, help="entry label, e.g. '#4257'")
    parser.add_argument(
        "--worktree",
        default="stack-build",
        help="build worktree directory name under <repo>/.worktrees",
    )
    args = parser.parse_args()

    worktree = f"{args.repo}/.worktrees/{args.worktree}"
    conflicted = git(worktree, "diff", "--name-only", "--diff-filter=U").split()
    if not conflicted:
        print("no conflicted files")
        return 0

    # Locate that entry's merge commit in the previous build.
    commit = git(
        args.repo, "log", "--format=%H", "--grep",
        f"^stack: merge {args.label} ", "--extended-regexp", args.from_build,
    ).splitlines()
    if not commit:
        print(f"error: no 'stack: merge {args.label}' commit in {args.from_build}",
              file=sys.stderr)
        return 1
    source = commit[0]

    missing = []
    for name in conflicted:
        blob = subprocess.run(
            ["git", "-C", args.repo, "cat-file", "-e", f"{source}:{name}"],
            capture_output=True,
        )
        if blob.returncode != 0:
            missing.append(name)
            continue
        content = subprocess.run(
            ["git", "-C", args.repo, "show", f"{source}:{name}"],
            capture_output=True,
        ).stdout
        Path(worktree, name).write_bytes(content)
        subprocess.run(["git", "-C", worktree, "add", name], check=True)
        print(f"  replayed  {name}")

    for name in missing:
        print(f"  MISSING   {name}  (not in {source[:9]})")

    print(f"\nreplayed {len(conflicted) - len(missing)} from {source[:9]}, "
          f"{len(missing)} missing")
    return 0 if not missing else 3


if __name__ == "__main__":
    raise SystemExit(main())
