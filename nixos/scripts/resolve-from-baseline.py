#!/usr/bin/env python3
"""Resolve stack-build merge conflicts using a known-good baseline tree.

For a conflicted file, if NO manifest entry after the current one touches that
file, then the baseline tree's version of it is exactly the correct post-merge
content -- every contribution to that file is already in the merge. Copying it
is both safe and semantically right.

If a LATER entry does touch the file, copying baseline would import that later
entry's changes early, which would make its own merge report EMPTY and produce
a false "drop candidate" signal. Those files are refused and must be resolved
by hand.

Usage:
  resolve-from-baseline.py --baseline /nix/store/...-t3code-patched-main-<date>
  resolve-from-baseline.py --baseline <path> --list   # report only, no writes
"""

from __future__ import annotations

import argparse
import json
import shutil
import subprocess
import sys
import tomllib
from pathlib import Path

DOTFILES = Path(__file__).resolve().parents[2]
MANIFEST = DOTFILES / "nix-shared" / "t3code-stack.toml"


def sh(repo: Path, *args: str) -> str:
    return subprocess.run(
        ["git", "-C", str(repo), *args], capture_output=True, text=True
    ).stdout.strip()


def touched_by_entry(repo: Path, main: str, pin: str) -> set[str]:
    oid = sh(repo, "rev-parse", f"{pin}^{{commit}}")
    if not oid:
        return set()
    return set(sh(repo, "diff", "--name-only", f"{main}...{oid}").splitlines())


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--baseline", required=True)
    parser.add_argument("--repo", default="/home/imalison/Projects/t3code")
    parser.add_argument("--manifest", default=str(MANIFEST))
    parser.add_argument("--worktree", default="stack-build")
    parser.add_argument(
        "--foreign-manifest",
        default=None,
        help=(
            "Another manifest (typically the main stack) whose entries are NOT "
            "part of this build. Any file they touch is refused: copying baseline "
            "would import their content into this branch early and make them "
            "falsely report EMPTY upstack. Use when building a GROUP manifest."
        ),
    )
    parser.add_argument("--list", action="store_true", help="report only")
    parser.add_argument(
        "--force",
        nargs="*",
        default=[],
        metavar="PATH",
        help=(
            "Resolve these paths from baseline even though a later entry touches "
            "them. Safe only when that later entry also changes OTHER files (so it "
            "cannot be falsely reported EMPTY); git then merges its identical "
            "changes cleanly. Verify before using."
        ),
    )
    args = parser.parse_args()

    repo = Path(args.repo)
    worktree = repo / ".worktrees" / args.worktree
    baseline = Path(args.baseline)
    manifest = tomllib.load(open(args.manifest, "rb"))
    entries = manifest["entry"]

    stem = Path(args.manifest).stem
    state_file = Path(sh(worktree, "rev-parse", "--git-dir")) / f"{stem}-state.json"
    if not state_file.is_absolute():
        state_file = worktree / state_file
    current = json.loads(state_file.read_text())["next_index"]
    main_oid = json.loads(state_file.read_text())["upstream_main"]

    later: set[str] = set()
    for entry in entries[current + 1 :]:
        if entry.get("pin"):
            later |= touched_by_entry(repo, main_oid, entry["pin"])

    # Files touched by entries that live outside this build entirely.
    if args.foreign_manifest:
        mine = {e.get("branch") or e.get("ref") for e in entries}
        foreign = tomllib.load(open(args.foreign_manifest, "rb"))["entry"]
        for entry in foreign:
            key = entry.get("branch") or entry.get("ref")
            if key in mine or not entry.get("pin"):
                continue
            later |= touched_by_entry(repo, main_oid, entry["pin"])

    conflicted = sh(worktree, "diff", "--name-only", "--diff-filter=U").split()
    if not conflicted:
        print("no conflicted files")
        return 0

    safe, unsafe = [], []
    for name in conflicted:
        if name in args.force:
            safe.append(name)
        else:
            (unsafe if name in later else safe).append(name)

    for name in safe:
        source = baseline / name
        if not source.exists():
            unsafe.append(name)
            continue
        if not args.list:
            target = worktree / name
            target.parent.mkdir(parents=True, exist_ok=True)
            shutil.copy2(source, target)
            subprocess.run(["git", "-C", str(worktree), "add", name], check=True)
        print(f"  baseline  {name}")

    for name in unsafe:
        print(f"  BY HAND   {name}  (a later entry also touches it)")

    print(f"\n{len(safe)} resolved from baseline, {len(unsafe)} need manual resolution")
    return 0 if not unsafe else 3


if __name__ == "__main__":
    raise SystemExit(main())
