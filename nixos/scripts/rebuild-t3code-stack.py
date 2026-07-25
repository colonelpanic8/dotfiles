#!/usr/bin/env python3
"""Rebuild the personal T3 Code integration branch from an ordered manifest.

The integration branch is a build artifact: it is regenerated from scratch on
every run by merging each manifest entry onto live upstream main, in order.
Nothing is ever committed to it directly and nothing is based on it.

Modes:
  reproduce  merge each entry at the OID recorded in the manifest/lock.
             Deterministic; used to prove a rebuild reproduces a known tree.
  refresh    merge each entry at its current branch head, picking up movement.

On a merge conflict the script stops, leaving the conflicted merge in the build
worktree for a human or agent to resolve, then resumes with --continue.
"""

from __future__ import annotations

import argparse
import json
import subprocess
import sys
import tomllib
from datetime import datetime, timezone
from pathlib import Path

DOTFILES = Path(__file__).resolve().parents[2]
SHARED = DOTFILES / "nix-shared"
DEFAULT_MANIFEST = SHARED / "t3code-stack.toml"

# Set from --manifest. A GROUP manifest (e.g. t3code-thread-picker.toml) is an
# ordinary manifest whose output branch is pinned as a single entry in the main
# one -- the same tool, two levels, like a subsystem tree under linux-next.
MANIFEST = DEFAULT_MANIFEST
LOCK = SHARED / "t3code-stack.lock.json"


def set_manifest(path: Path) -> None:
    global MANIFEST, LOCK, STATE_NAME, WORKTREE_NAME
    MANIFEST = path.resolve()
    LOCK = MANIFEST.parent / f"{MANIFEST.stem}.lock.json"
    # Per-manifest worktree and state, so a group build and the main build can
    # be in progress at the same time without clobbering each other.
    STATE_NAME = f"{MANIFEST.stem}-state.json"
    WORKTREE_NAME = f"{MANIFEST.stem}-build"


STATE_NAME = "t3code-stack-state.json"
WORKTREE_NAME = "t3code-stack-build"


class Fail(Exception):
    pass


def git(repo: Path, *args: str, check: bool = True, capture: bool = True) -> str:
    proc = subprocess.run(
        ["git", "-C", str(repo), *args],
        capture_output=capture,
        text=True,
    )
    if check and proc.returncode != 0:
        raise Fail(
            f"git {' '.join(args)} failed ({proc.returncode})\n"
            f"{(proc.stderr or '').strip()}"
        )
    return (proc.stdout or "").strip()


def git_ok(repo: Path, *args: str) -> bool:
    return subprocess.run(
        ["git", "-C", str(repo), *args],
        capture_output=True,
        text=True,
    ).returncode == 0


def load_manifest() -> dict:
    with MANIFEST.open("rb") as handle:
        return tomllib.load(handle)


def entry_label(entry: dict) -> str:
    if entry.get("pr"):
        return f"#{entry['pr']}"
    return entry.get("branch", entry.get("ref", "<unnamed>"))


def resolve_entry(repo: Path, entry: dict, mode: str) -> str:
    """Return the commit OID this entry should be merged at."""
    kind = entry.get("kind")
    pin = entry.get("pin")

    if mode == "reproduce":
        if not pin:
            raise Fail(
                f"{entry_label(entry)}: reproduce mode needs a `pin`, none recorded"
            )
        # External PR heads are not reachable from any local branch, so the
        # pinned OID may simply not be fetched yet.
        if kind == "external" and not git_ok(repo, "cat-file", "-e", f"{pin}^{{commit}}"):
            ref = entry["ref"]
            git(repo, "fetch", "origin",
                f"+{ref}:refs/t3code-stack/{ref.replace('/', '-')}", check=False)
        if not git_ok(repo, "cat-file", "-e", f"{pin}^{{commit}}"):
            raise Fail(
                f"{entry_label(entry)}: pinned OID {pin} not present locally; "
                "fetch the branch or the PR ref first"
            )
        return git(repo, "rev-parse", f"{pin}^{{commit}}")

    if kind == "external":
        ref = entry["ref"]
        git(repo, "fetch", "origin", f"+{ref}:refs/t3code-stack/{ref.replace('/', '-')}")
        return git(repo, "rev-parse", f"refs/t3code-stack/{ref.replace('/', '-')}")

    branch = entry["branch"]
    # Local topics live only in the local checkout; fork topics track the remote.
    candidates = [branch] if kind == "local" else [f"fork/{branch}", branch]
    for candidate in candidates:
        if git_ok(repo, "rev-parse", "--verify", candidate):
            return git(repo, "rev-parse", candidate)
    raise Fail(f"{entry_label(entry)}: none of {candidates} resolve")


def check_absorbed(repo: Path, oid: str, main: str) -> bool:
    """True when upstream already contains this entry -- a drop candidate."""
    return git_ok(repo, "merge-base", "--is-ancestor", oid, main)


def prepare_worktree(repo: Path, path: Path, main: str) -> None:
    if path.exists():
        git(repo, "worktree", "remove", "--force", str(path), check=False)
    path.parent.mkdir(parents=True, exist_ok=True)
    git(repo, "worktree", "add", "--detach", str(path), main)


def read_state(repo: Path, worktree: Path) -> dict | None:
    common = Path(git(worktree, "rev-parse", "--git-dir"))
    if not common.is_absolute():
        common = worktree / common
    candidate = common / STATE_NAME
    if candidate.exists():
        return json.loads(candidate.read_text())
    return None


def write_state(worktree: Path, state: dict) -> None:
    common = Path(git(worktree, "rev-parse", "--git-dir"))
    if not common.is_absolute():
        common = worktree / common
    (common / STATE_NAME).write_text(json.dumps(state, indent=2))


def merge_entry(worktree: Path, entry: dict, oid: str) -> bool:
    """Merge one entry. Returns True on clean merge, False if conflicted."""
    label = entry_label(entry)
    message = f"stack: merge {label} ({entry.get('summary', '')})".strip()
    proc = subprocess.run(
        [
            "git", "-C", str(worktree), "merge", "--no-ff", "--no-edit",
            "-m", message, oid,
        ],
        capture_output=True,
        text=True,
    )
    return proc.returncode == 0


def conflicted_files(worktree: Path) -> list[str]:
    out = git(worktree, "diff", "--name-only", "--diff-filter=U")
    return [line for line in out.splitlines() if line]


def run(args: argparse.Namespace) -> int:
    manifest = load_manifest()
    repo = Path(args.repo).resolve()
    worktree = repo / ".worktrees" / WORKTREE_NAME
    entries = manifest["entry"]
    epilogues = manifest.get("epilogue", [])

    if args.cont:
        state = read_state(repo, worktree)
        if not state:
            raise Fail("no in-progress rebuild found; run without --continue")
        if conflicted_files(worktree):
            raise Fail(
                "the build worktree still has unresolved conflicts:\n  "
                + "\n  ".join(conflicted_files(worktree))
            )
        # Commit the resolved merge if one is in progress.
        common = Path(git(worktree, "rev-parse", "--git-dir"))
        if not common.is_absolute():
            common = worktree / common
        main = state["upstream_main"]
        mode = state["mode"]
        results = state["results"]
        conflicts = state["conflicts"]
        stalled = state["next_index"]

        if (common / "MERGE_HEAD").exists():
            git(worktree, "commit", "--no-edit")
            # The stalled entry is now merged; record it and advance past it.
            # Re-running it would be a no-op merge that falsely reports EMPTY.
            entry = entries[stalled]
            results.append(
                {
                    "entry": entry_label(entry),
                    "oid": resolve_entry(repo, entry, mode),
                    "status": "merged",
                    "conflicted": True,
                }
            )
            print(f"  [{stalled + 1:2}/{len(entries)}] {entry_label(entry):<10} resolved and committed")
            start = stalled + 1
            # Persist immediately: if the very next entry raises (e.g. a bad
            # pin), a stale index would re-merge this one and report it EMPTY.
            write_state(
                worktree,
                {
                    "next_index": start,
                    "upstream_main": main,
                    "mode": mode,
                    "results": results,
                    "conflicts": conflicts,
                },
            )
        else:
            start = stalled
        print(f"resuming at entry {start + 1}/{len(entries)}")
    else:
        print("fetching upstream main and fork...")
        git(repo, "fetch", "origin", "main", capture=False)
        git(repo, "fetch", "fork", capture=False)
        main = git(repo, "rev-parse", "origin/main")
        mode = args.mode
        results = []
        conflicts = 0
        start = 0
        print(f"upstream main: {main}")
        prepare_worktree(repo, worktree, main)

    for index in range(start, len(entries)):
        entry = entries[index]
        label = entry_label(entry)
        oid = resolve_entry(repo, entry, mode)

        if check_absorbed(repo, oid, main):
            print(f"  [{index + 1:2}/{len(entries)}] {label:<10} ABSORBED upstream -- drop candidate")
            results.append({"entry": label, "oid": oid, "status": "absorbed"})
            continue

        before = git(worktree, "rev-parse", "HEAD^{tree}")
        clean = merge_entry(worktree, entry, oid)

        if not clean:
            files = conflicted_files(worktree)
            conflicts += 1
            write_state(
                worktree,
                {
                    "next_index": index,
                    "upstream_main": main,
                    "mode": mode,
                    "results": results,
                    "conflicts": conflicts,
                },
            )
            print(f"\n  [{index + 1:2}/{len(entries)}] {label} CONFLICT in {len(files)} file(s):")
            for name in files:
                print(f"      {name}")
            print(f"\n  Resolve in: {worktree}")
            print(f"  Then: {sys.argv[0]} --continue")
            return 2

        after = git(worktree, "rev-parse", "HEAD^{tree}")
        if before == after:
            print(f"  [{index + 1:2}/{len(entries)}] {label:<10} EMPTY -- merge changed nothing, drop candidate")
            results.append({"entry": label, "oid": oid, "status": "empty"})
        else:
            print(f"  [{index + 1:2}/{len(entries)}] {label:<10} merged {oid[:9]}")
            results.append({"entry": label, "oid": oid, "status": "merged"})

        # Persist after EVERY entry, not just on conflict. Otherwise a crash
        # mid-run resumes at a stale index, re-merges already-merged entries,
        # and falsely reports them EMPTY -- corrupting the drop-candidate signal.
        write_state(
            worktree,
            {
                "next_index": index + 1,
                "upstream_main": main,
                "mode": mode,
                "results": results,
                "conflicts": conflicts,
            },
        )

    # Epilogues are patches, not topics: they are functions of the ASSEMBLED
    # tree (e.g. a migration ID that depends on what the stack already used),
    # so they cannot exist as branches based on upstream main.
    for epilogue in epilogues:
        patch = DOTFILES / "nix-shared" / epilogue["patch"]
        label = epilogue.get("summary", patch.name)
        proc = subprocess.run(
            ["git", "-C", str(worktree), "apply", "--3way", str(patch)],
            capture_output=True,
            text=True,
        )
        if proc.returncode != 0:
            print(f"  epilogue {patch.name} FAILED to apply:\n{proc.stderr}")
            print(f"  Resolve in: {worktree}")
            return 2
        git(worktree, "add", "-A")
        if not git(worktree, "diff", "--cached", "--name-only"):
            print(f"  epilogue {patch.name} already applied, skipping")
            continue
        git(
            worktree, "-c", "user.name=Ivan Malison",
            "-c", "user.email=IvanMalison@gmail.com",
            "commit", "-q", "-m", f"stack: {label}",
        )
        print(f"  epilogue {patch.name} applied")
        results.append({"entry": patch.name, "status": "epilogue"})

    tree = git(worktree, "rev-parse", "HEAD^{tree}")
    head = git(worktree, "rev-parse", "HEAD")

    previous_tree = None
    if LOCK.exists():
        previous_tree = json.loads(LOCK.read_text()).get("tree")

    lock = {
        "generated": datetime.now(timezone.utc).isoformat(),
        "mode": mode,
        "upstream_main": main,
        "integration_branch": manifest["integration_branch"],
        "commit": head,
        "tree": tree,
        "previous_tree": previous_tree,
        "tree_changed": previous_tree != tree,
        "conflicts": conflicts,
        "entries": results,
    }

    print(f"\ntree:   {tree}")
    print(f"commit: {head}")
    print(f"conflicts resolved this run: {conflicts}")
    if previous_tree and previous_tree == tree:
        print("tree UNCHANGED from previous lock -- no flake bump needed")
    elif previous_tree:
        print(f"tree CHANGED (was {previous_tree})")

    if args.write_lock:
        LOCK.write_text(json.dumps(lock, indent=2) + "\n")
        print(f"wrote {LOCK}")
    else:
        print("(--write-lock not given; lock not written)")

    if args.push:
        branch = manifest["integration_branch"]
        stamp = datetime.now(timezone.utc).strftime("%Y%m%d-%H%M%S")
        tag = f"{manifest['tag_prefix']}/{stamp}"
        git(repo, "push", "--force", "fork", f"{head}:refs/heads/{branch}", capture=False)
        git(repo, "push", "fork", f"{head}:refs/tags/{tag}", capture=False)
        print(f"pushed fork/{branch} and tag {tag}")
        print("Pin the flake input by REV, never by branch -- the branch is force-pushed.")

    return 0


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--repo",
        default="/home/imalison/Projects/t3code",
        help="path to the T3 Code checkout",
    )
    parser.add_argument(
        "--mode",
        choices=["reproduce", "refresh"],
        default="refresh",
        help="reproduce: merge at manifest pins. refresh: merge at current heads.",
    )
    parser.add_argument(
        "--manifest",
        type=Path,
        default=DEFAULT_MANIFEST,
        help=(
            "Manifest to build. A group manifest (e.g. t3code-thread-picker.toml) "
            "produces a branch that the main manifest then pins as one entry. "
            "Lock, state, and build worktree are all derived from this name."
        ),
    )
    parser.add_argument("--continue", dest="cont", action="store_true")
    parser.add_argument("--push", action="store_true")
    parser.add_argument("--write-lock", action="store_true")
    args = parser.parse_args()
    set_manifest(args.manifest)

    try:
        return run(args)
    except Fail as error:
        print(f"error: {error}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
