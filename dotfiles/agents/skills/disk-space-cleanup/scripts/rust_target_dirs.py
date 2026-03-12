#!/usr/bin/env python3

import argparse
import json
import os
import shutil
import subprocess
import sys
import time
from pathlib import Path


SCRIPT_DIR = Path(__file__).resolve().parent
DEFAULT_ROOTS_FILE = SCRIPT_DIR.parent / "references" / "rust-target-roots.txt"


def parse_size(value: str) -> int:
    text = value.strip().upper()
    units = {
        "B": 1,
        "K": 1024,
        "KB": 1024,
        "M": 1024**2,
        "MB": 1024**2,
        "G": 1024**3,
        "GB": 1024**3,
        "T": 1024**4,
        "TB": 1024**4,
    }
    for suffix, multiplier in units.items():
        if text.endswith(suffix):
            number = text[: -len(suffix)].strip()
            return int(float(number) * multiplier)
    return int(float(text))


def human_size(num_bytes: int) -> str:
    value = float(num_bytes)
    for unit in ["B", "K", "M", "G", "T"]:
        if value < 1024 or unit == "T":
            if unit == "B":
                return f"{int(value)}B"
            return f"{value:.1f}{unit}"
        value /= 1024
    return f"{num_bytes}B"


def is_relative_to(path: Path, root: Path) -> bool:
    try:
        path.relative_to(root)
        return True
    except ValueError:
        return False


def load_roots(roots_file: Path, cli_roots: list[str]) -> list[Path]:
    roots: list[Path] = []
    for raw in cli_roots:
        candidate = Path(raw).expanduser().resolve()
        if candidate.exists():
            roots.append(candidate)

    if roots_file.exists():
        for line in roots_file.read_text().splitlines():
            stripped = line.split("#", 1)[0].strip()
            if not stripped:
                continue
            candidate = Path(stripped).expanduser().resolve()
            if candidate.exists():
                roots.append(candidate)

    unique_roots: list[Path] = []
    seen: set[Path] = set()
    for root in roots:
        if root not in seen:
            unique_roots.append(root)
            seen.add(root)
    return unique_roots


def du_size_bytes(path: Path) -> int:
    result = subprocess.run(
        ["du", "-sb", str(path)],
        check=True,
        capture_output=True,
        text=True,
    )
    return int(result.stdout.split()[0])


def nearest_cargo_root(path: Path, stop_roots: list[Path]) -> str:
    current = path.parent
    stop_root_set = set(stop_roots)
    while current != current.parent:
        if (current / "Cargo.toml").exists():
            return str(current)
        if current in stop_root_set:
            break
        current = current.parent
    return ""


def discover_targets(roots: list[Path]) -> list[dict]:
    results: dict[Path, dict] = {}
    now = time.time()
    for root in roots:
        for current, dirnames, _filenames in os.walk(root, topdown=True):
            if "target" in dirnames:
                target_dir = (Path(current) / "target").resolve()
                dirnames.remove("target")
                if target_dir in results or not target_dir.is_dir():
                    continue
                stat_result = target_dir.stat()
                size_bytes = du_size_bytes(target_dir)
                age_days = int((now - stat_result.st_mtime) // 86400)
                results[target_dir] = {
                    "path": str(target_dir),
                    "size_bytes": size_bytes,
                    "size_human": human_size(size_bytes),
                    "age_days": age_days,
                    "workspace": nearest_cargo_root(target_dir, roots),
                }
    return sorted(results.values(), key=lambda item: item["size_bytes"], reverse=True)


def print_table(rows: list[dict]) -> None:
    if not rows:
        print("No matching Rust target directories found.")
        return
    size_width = max(len(row["size_human"]) for row in rows)
    age_width = max(len(str(row["age_days"])) for row in rows)
    print(
        f"{'SIZE'.ljust(size_width)}  {'AGE'.rjust(age_width)}  PATH"
    )
    for row in rows:
        print(
            f"{row['size_human'].ljust(size_width)}  "
            f"{str(row['age_days']).rjust(age_width)}d  "
            f"{row['path']}"
        )


def filter_rows(rows: list[dict], min_size: int, older_than: int | None, limit: int | None) -> list[dict]:
    filtered = [row for row in rows if row["size_bytes"] >= min_size]
    if older_than is not None:
        filtered = [row for row in filtered if row["age_days"] >= older_than]
    if limit is not None:
        filtered = filtered[:limit]
    return filtered


def cmd_list(args: argparse.Namespace) -> int:
    roots = load_roots(Path(args.roots_file).expanduser(), args.root)
    if not roots:
        print("No scan roots available.", file=sys.stderr)
        return 1
    rows = discover_targets(roots)
    rows = filter_rows(rows, parse_size(args.min_size), args.older_than, args.limit)

    if args.output == "json":
        print(json.dumps(rows, indent=2))
    elif args.output == "tsv":
        for row in rows:
            print(
                "\t".join(
                    [
                        str(row["size_bytes"]),
                        str(row["age_days"]),
                        row["path"],
                        row["workspace"],
                    ]
                )
            )
    elif args.output == "paths":
        for row in rows:
            print(row["path"])
    else:
        print_table(rows)
    return 0


def validate_delete_path(path_text: str, roots: list[Path]) -> Path:
    target = Path(path_text).expanduser().resolve(strict=True)
    if target.name != "target":
        raise ValueError(f"{target} is not a target directory")
    if target.is_symlink():
        raise ValueError(f"{target} is a symlink")
    if not target.is_dir():
        raise ValueError(f"{target} is not a directory")
    if not any(is_relative_to(target, root) for root in roots):
        raise ValueError(f"{target} is outside configured scan roots")
    if nearest_cargo_root(target, roots) == "":
        raise ValueError(f"{target} is not beneath a Cargo project")
    return target


def cmd_delete(args: argparse.Namespace) -> int:
    roots = load_roots(Path(args.roots_file).expanduser(), args.root)
    if not roots:
        print("No scan roots available.", file=sys.stderr)
        return 1

    targets: list[Path] = []
    for raw_path in args.path:
        try:
            targets.append(validate_delete_path(raw_path, roots))
        except ValueError as exc:
            print(str(exc), file=sys.stderr)
            return 1

    total_size = sum(du_size_bytes(target) for target in targets)
    print(f"Matched {len(targets)} target directories totaling {human_size(total_size)}:")
    for target in targets:
        print(str(target))

    if not args.yes:
        print("Dry run only. Re-run with --yes to delete these target directories.")
        return 0

    for target in targets:
        shutil.rmtree(target)
    print(f"Deleted {len(targets)} target directories.")
    return 0


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Inventory and delete Rust target directories under configured roots."
    )
    parser.add_argument(
        "--roots-file",
        default=str(DEFAULT_ROOTS_FILE),
        help="Path to the newline-delimited root list.",
    )
    parser.add_argument(
        "--root",
        action="append",
        default=[],
        help="Additional root to scan. May be provided multiple times.",
    )

    subparsers = parser.add_subparsers(dest="command", required=True)

    list_parser = subparsers.add_parser("list", help="List target directories.")
    list_parser.add_argument("--min-size", default="0", help="Minimum size threshold, for example 500M or 2G.")
    list_parser.add_argument("--older-than", type=int, help="Only include targets at least this many days old.")
    list_parser.add_argument("--limit", type=int, help="Maximum number of rows to print.")
    list_parser.add_argument(
        "--output",
        choices=["table", "tsv", "json", "paths"],
        default="table",
        help="Output format.",
    )
    list_parser.set_defaults(func=cmd_list)

    delete_parser = subparsers.add_parser("delete", help="Delete explicit target directories.")
    delete_parser.add_argument("path", nargs="+", help="One or more target directories to delete.")
    delete_parser.add_argument("--yes", action="store_true", help="Actually delete the paths.")
    delete_parser.set_defaults(func=cmd_delete)

    return parser


def main() -> int:
    parser = build_parser()
    args = parser.parse_args()
    return args.func(args)


if __name__ == "__main__":
    raise SystemExit(main())
