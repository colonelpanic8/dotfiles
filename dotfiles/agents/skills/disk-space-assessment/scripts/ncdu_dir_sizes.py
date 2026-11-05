"""Stream an ncdu JSON export and print per-directory disk usage as TSV.

Use for snapshots too large for `safe_ncdu top`. Hardlinked inodes are counted
once, at the first path ncdu exported, so pnpm stores and their node_modules
are not double counted.

    zstdcat SNAPSHOT.json.zst \
      | nix shell --impure --expr 'with import <nixpkgs> {}; python3.withPackages (p: [p.ijson])' \
          --command python3 -I ncdu_dir_sizes.py [MAX_DEPTH] [MIN_BYTES] > dirs.tsv

Output rows are `<bytes>\t<path>`; `/` is depth 0.
"""

import sys

import ijson


def main():
    max_depth = int(sys.argv[1]) if len(sys.argv) > 1 else 12
    min_bytes = int(sys.argv[2]) if len(sys.argv) > 2 else 0
    stack = []  # [path, bytes, depth], or None until the dir's info map is read
    seen_inodes = set()
    info = {}
    key = None
    array_depth = 0
    out = sys.stdout

    for _prefix, event, value in ijson.parse(sys.stdin.buffer):
        if event == "start_array":
            array_depth += 1
            if array_depth >= 2:
                stack.append(None)
        elif event == "end_array":
            array_depth -= 1
            if array_depth >= 1 and stack:
                entry = stack.pop()
                if entry is not None:
                    path, size, depth = entry
                    if depth <= max_depth and size >= min_bytes:
                        out.write(f"{size}\t{path}\n")
                    if stack and stack[-1] is not None:
                        stack[-1][1] += size
        elif event == "start_map":
            info = {}
            key = None
        elif event == "map_key":
            key = value
        elif event == "end_map":
            if array_depth < 2:
                continue
            size = int(info.get("dsize", 0))
            if info.get("hlnkc"):
                inode = (info.get("dev"), info.get("ino"))
                if inode in seen_inodes:
                    size = 0
                seen_inodes.add(inode)
            if stack and stack[-1] is None:
                parent = stack[-2] if len(stack) >= 2 else None
                name = info.get("name", "?")
                if parent is None:
                    stack[-1] = [name, size, 0]
                else:
                    stack[-1] = [parent[0].rstrip("/") + "/" + name, size, parent[2] + 1]
            elif stack and stack[-1] is not None:
                stack[-1][1] += size
        elif key is not None:
            info[key] = value


if __name__ == "__main__":
    main()
