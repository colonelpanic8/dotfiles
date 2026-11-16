"""Per-directory disk usage from an ncdu 2.x export, as `<bytes>\t<path>` TSV.

ncdu 2.x writes one entry per line, so this parses line by line instead of
streaming JSON: a 206 MB compressed root snapshot takes ~50 s. Hardlinked
inodes count once, at the first path exported.

    zstdcat SNAPSHOT.json.zst | python3 -I ncdu_dir_sizes.py [MAX_DEPTH] [MIN_BYTES] > dirs.tsv
"""
import json
import sys

max_depth = int(sys.argv[1]) if len(sys.argv) > 1 else 12
min_bytes = int(sys.argv[2]) if len(sys.argv) > 2 else 0
stack = []  # [path, bytes]
seen = set()
out = sys.stdout
dup_bytes = 0

for raw in sys.stdin:
    line = raw.rstrip("\n")
    if line.endswith(","):
        line = line[:-1]
    opens = 0
    while line.startswith("["):
        opens += 1
        line = line[1:]
    if not line.startswith("{"):
        continue
    end = line.rfind("}")
    closes = line[end + 1:].count("]")
    if opens > 0 and stack == [] and '"progname"' in line:
        continue
    info = json.loads(line[: end + 1])
    size = int(info.get("dsize", 0))
    if info.get("hlnkc") or int(info.get("nlink", 1)) > 1:
        key = (info.get("dev"), info.get("ino"))
        if key in seen:
            dup_bytes += size
            size = 0
        else:
            seen.add(key)
    name = info.get("name", "?")
    if opens:
        parent = stack[-1][0] if stack else ""
        path = name if not stack else parent.rstrip("/") + "/" + name
        stack.append([path, size])
    elif stack:
        stack[-1][1] += size
    for _ in range(closes):
        if not stack:
            break
        path, sz = stack.pop()
        depth = 0 if path == "/" else path.count("/")
        if depth <= max_depth and sz >= min_bytes:
            out.write(f"{sz}\t{path}\n")
        if stack:
            stack[-1][1] += sz
sys.stderr.write(f"hardlink duplicate bytes skipped: {dup_bytes}\n")
