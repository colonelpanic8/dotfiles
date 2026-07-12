#!/usr/bin/env python3

import argparse
import collections
import datetime as dt
import json
import os
import socket
import sqlite3
import subprocess
from pathlib import Path


DB_PATH = Path("/nix/var/nix/db/db.sqlite")


def run(*args: str) -> str:
    return subprocess.run(args, check=True, text=True, capture_output=True).stdout


def chunks(values: list[str], size: int = 100):
    for index in range(0, len(values), size):
        yield values[index : index + size]


def closure(targets: set[str]) -> set[str]:
    result: set[str] = set()
    existing = sorted(target for target in targets if Path(target).exists())
    for batch in chunks(existing):
        result.update(line for line in run("nix-store", "-qR", *batch).splitlines() if line)
    return result


def sizes(paths: set[str]) -> dict[str, int]:
    if not paths:
        return {}
    connection = sqlite3.connect(f"file:{DB_PATH}?mode=ro", uri=True)
    try:
        result: dict[str, int] = {}
        ordered = sorted(paths)
        for batch in chunks(ordered, 900):
            placeholders = ",".join("?" for _ in batch)
            query = f"select path, narSize from ValidPaths where path in ({placeholders})"
            result.update({path: int(size or 0) for path, size in connection.execute(query, batch)})
        return result
    finally:
        connection.close()


def human_size(value: int) -> str:
    amount = float(value)
    for unit in ("B", "KiB", "MiB", "GiB", "TiB"):
        if amount < 1024 or unit == "TiB":
            return f"{amount:.2f} {unit}"
        amount /= 1024
    raise AssertionError("unreachable")


def parse_roots() -> list[tuple[str, str]]:
    entries: list[tuple[str, str]] = []
    for line in run("nix-store", "--gc", "--print-roots").splitlines():
        if " -> " not in line:
            continue
        source, target = line.split(" -> ", 1)
        source = source.strip('"')
        target = target.strip('"')
        if target.startswith("/nix/store/"):
            entries.append((source, target))
    return entries


def project_for_root(source: str) -> str | None:
    marker = "/.direnv/"
    if marker not in source:
        return None
    return source.split(marker, 1)[0]


def top_paths(paths: set[str], path_sizes: dict[str, int], limit: int) -> list[dict]:
    ranked = sorted(paths, key=lambda path: path_sizes.get(path, 0), reverse=True)[:limit]
    return [{"path": path, "nar_size_bytes": path_sizes.get(path, 0)} for path in ranked]


def main() -> int:
    parser = argparse.ArgumentParser(description="Audit Nix paths retained by .direnv GC roots.")
    parser.add_argument("--top", type=int, default=30, help="Projects and store paths to show.")
    parser.add_argument("--output", help="JSON artifact path; defaults under ~/.cache/ncdu.")
    args = parser.parse_args()
    if args.top < 1:
        parser.error("--top must be positive")
    if not DB_PATH.is_file():
        parser.error(f"Nix database not found: {DB_PATH}")

    generated_at = dt.datetime.now().astimezone()
    roots = parse_roots()
    grouped: dict[str, list[tuple[str, str]]] = collections.defaultdict(list)
    non_direnv_targets: set[str] = set()
    for source, target in roots:
        project = project_for_root(source)
        if project is None:
            non_direnv_targets.add(target)
        else:
            grouped[project].append((source, target))

    project_closures = {
        project: closure({target for _source, target in entries})
        for project, entries in sorted(grouped.items())
    }
    non_direnv_closure = closure(non_direnv_targets)
    all_direnv_closure = set().union(*project_closures.values()) if project_closures else set()
    direnv_only = all_direnv_closure - non_direnv_closure

    membership: collections.Counter[str] = collections.Counter()
    for project_paths in project_closures.values():
        membership.update(project_paths)

    all_relevant = all_direnv_closure | non_direnv_closure
    path_sizes = sizes(all_relevant)
    total = lambda paths: sum(path_sizes.get(path, 0) for path in paths)

    projects = []
    for project, entries in grouped.items():
        project_paths = project_closures[project]
        outside_non_direnv = project_paths - non_direnv_closure
        marginal = {path for path in outside_non_direnv if membership[path] == 1}
        direnv_path = Path(project) / ".direnv"
        mtime = None
        age_days = None
        if direnv_path.exists():
            timestamp = direnv_path.stat().st_mtime
            mtime = dt.datetime.fromtimestamp(timestamp).astimezone().isoformat()
            age_days = (generated_at.timestamp() - timestamp) / 86400
        projects.append(
            {
                "project": project,
                "direnv_mtime": mtime,
                "direnv_age_days": age_days,
                "raw_root_count": len(entries),
                "unique_target_count": len({target for _source, target in entries}),
                "closure_path_count": len(project_paths),
                "closure_nar_bytes": total(project_paths),
                "outside_non_direnv_nar_bytes": total(outside_non_direnv),
                "marginal_unique_nar_bytes": total(marginal),
                "shared_with_other_direnv_nar_bytes": total(outside_non_direnv - marginal),
                "roots": [{"source": source, "target": target} for source, target in entries],
                "top_marginal_paths": top_paths(marginal, path_sizes, args.top),
            }
        )
    projects.sort(key=lambda item: (item["marginal_unique_nar_bytes"], item["closure_nar_bytes"]), reverse=True)

    shared_direnv_only = {path for path in direnv_only if membership[path] > 1}
    shared_ranked = sorted(shared_direnv_only, key=lambda path: path_sizes.get(path, 0), reverse=True)[: args.top]
    top_shared = [
        {
            "path": path,
            "nar_size_bytes": path_sizes.get(path, 0),
            "retained_by_projects": sorted(
                project for project, project_paths in project_closures.items() if path in project_paths
            ),
        }
        for path in shared_ranked
    ]
    marginal_total = sum(item["marginal_unique_nar_bytes"] for item in projects)

    artifact = {
        "format_version": 1,
        "generated_at": generated_at.isoformat(),
        "hostname": socket.gethostname(),
        "measurement": "logical NAR size from the Nix database",
        "raw_gc_root_count": len(roots),
        "raw_direnv_root_count": sum(len(entries) for entries in grouped.values()),
        "unique_direnv_target_count": len({target for entries in grouped.values() for _source, target in entries}),
        "direnv_project_count": len(grouped),
        "all_direnv_closure_nar_bytes": total(all_direnv_closure),
        "collectively_direnv_only_nar_bytes": total(direnv_only),
        "collectively_direnv_only_path_count": len(direnv_only),
        "marginal_unique_total_nar_bytes": marginal_total,
        "shared_direnv_only_nar_bytes": total(shared_direnv_only),
        "top_collectively_direnv_only_paths": top_paths(direnv_only, path_sizes, args.top),
        "top_shared_direnv_only_paths": top_shared,
        "projects": projects,
    }

    out_dir = Path.home() / ".cache" / "ncdu"
    out_dir.mkdir(parents=True, exist_ok=True)
    output = Path(args.output).expanduser() if args.output else out_dir / f"direnv-gc-roots-{generated_at:%Y%m%d-%H%M%S}.json"
    output = output.resolve()
    output.parent.mkdir(parents=True, exist_ok=True)
    temporary = output.with_suffix(output.suffix + ".tmp")
    temporary.write_text(json.dumps(artifact, indent=2) + "\n")
    os.replace(temporary, output)
    latest = out_dir / "latest-direnv-gc-roots.json"
    latest.unlink(missing_ok=True)
    latest.symlink_to(output)

    print(f"Direnv GC-root artifact: {output}")
    print(f"Raw direnv roots: {artifact['raw_direnv_root_count']}")
    print(f"Unique direnv targets: {artifact['unique_direnv_target_count']}")
    print(f"Projects: {artifact['direnv_project_count']}")
    print(f"All direnv closures: {human_size(artifact['all_direnv_closure_nar_bytes'])}")
    print(f"Collectively direnv-only: {human_size(artifact['collectively_direnv_only_nar_bytes'])}")
    print(f"Sum of project marginal unique: {human_size(artifact['marginal_unique_total_nar_bytes'])}")
    print(f"Shared by multiple direnv projects only: {human_size(artifact['shared_direnv_only_nar_bytes'])}")
    print()
    print(f"{'MARGINAL':>11}  {'CLOSURE':>11}  {'AGE(d)':>8}  PROJECT")
    for item in projects[: args.top]:
        age = "?" if item["direnv_age_days"] is None else f"{item['direnv_age_days']:.1f}"
        print(
            f"{human_size(item['marginal_unique_nar_bytes']):>11}  "
            f"{human_size(item['closure_nar_bytes']):>11}  {age:>8}  {item['project']}"
        )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
