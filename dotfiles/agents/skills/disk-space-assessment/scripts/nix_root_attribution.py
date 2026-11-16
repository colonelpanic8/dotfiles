"""Attribute live Nix store bytes to GC-root groups and price stale-root cleanup.

Usage (roots need sudo for full coverage):
    sudo -n nix-store --gc --print-roots > roots.txt
    nix-store --gc --print-dead > dead.txt
    python3 -I nix_root_attribution.py roots.txt dead.txt

Closure follows references, deriver (keep-derivations) and derivation outputs
(keep-outputs). Prints, per group, bytes reachable only from that group.
"""
import re
import sqlite3
import sys
from collections import defaultdict

roots_file, dead_file = sys.argv[1], sys.argv[2]
db = sqlite3.connect("file:/nix/var/nix/db/db.sqlite?mode=ro", uri=True)
path_id, nar, deriver = {}, {}, {}
for i, p, n, d in db.execute("select id, path, narSize, deriver from ValidPaths"):
    path_id[p] = i
    nar[i] = n or 0
    if d:
        deriver[i] = d
edges = defaultdict(list)
for a, b in db.execute("select referrer, reference from Refs"):
    if a != b:
        edges[a].append(b)
for i, d in deriver.items():
    if d in path_id:
        edges[i].append(path_id[d])
for drv, out in db.execute("select drv, path from DerivationOutputs"):
    if out in path_id:
        edges[drv].append(path_id[out])

current = None
gens = {}
for line in open(roots_file):
    m = re.match(r'^"?(.*?)"? -> (/nix/store/\S+)', line.strip())
    if m and m.group(1) == "/nix/var/nix/profiles/system":
        current = m.group(2)


def group(link, target):
    if link.startswith("/proc/") or link.startswith("{temp"):
        return "live-processes"
    if link in ("/run/current-system", "/nix/var/nix/profiles/system"):
        return "current-system"
    if link == "/run/booted-system":
        return "booted-system"
    m = re.match(r"/nix/var/nix/profiles/system-(\d+)-link", link)
    if m:
        return "current-system" if target == current else "old-system-generations"
    m = re.match(r"(/home/[^/]+)/\.local/state/(.*)", link)
    if m:
        return f"hm:{m.group(1)}:{m.group(2).split('/')[0]}/{m.group(2).split('/')[-1][:40]}"
    m = re.match(r"(.*?)/(\.direnv|result[^/]*|\.pre-commit-config\.yaml|out-link)(/|$)", link)
    if m:
        return f"{m.group(2).split('-')[0] if m.group(2).startswith('result') else m.group(2)}:{m.group(1)}"
    return "other:" + link


owners = defaultdict(set)
for line in open(roots_file):
    m = re.match(r'^"?(.*?)"? -> (/nix/store/\S+)', line.strip())
    if not m:
        continue
    link, target = m.groups()
    if target in path_id:
        owners[group(link, target)].add(path_id[target])

count, first = defaultdict(int), {}
closure_size = {}
for g, starts in owners.items():
    seen = set(starts)
    todo = list(starts)
    while todo:
        x = todo.pop()
        for y in edges.get(x, ()):
            if y not in seen:
                seen.add(y)
                todo.append(y)
    closure_size[g] = sum(nar[x] for x in seen)
    for x in seen:
        count[x] += 1
        first.setdefault(x, g)

unique = defaultdict(int)
for x, c in count.items():
    if c == 1:
        unique[first[x]] += nar[x]
live = sum(nar[x] for x in count)
total = sum(nar.values())
dead = sum(nar[path_id[p.strip()]] for p in open(dead_file) if p.strip() in path_id)
G = 2**30
print(f"store logical {total/G:.1f}G  live {live/G:.1f}G  unreached {(total-live)/G:.1f}G  print-dead {dead/G:.1f}G")
print(f"shared by >1 group: {sum(nar[x] for x,c in count.items() if c>1)/G:.1f}G")
for g, b in sorted(unique.items(), key=lambda kv: -kv[1])[:45]:
    print(f"{b/G:8.1f}G unique  {closure_size[g]/G:7.1f}G closure  {g}")

import os, time
def clos(starts):
    seen = set(starts); todo = list(starts)
    while todo:
        x = todo.pop()
        for y in edges.get(x, ()):
            if y not in seen:
                seen.add(y); todo.append(y)
    return seen
prot_groups = [g for g in owners if g in ("current-system", "booted-system", "live-processes") or g.endswith("current-home")]
prot = clos(set().union(*(owners[g] for g in prot_groups)))
print(f"\nprotected (current+booted+procs+hm current): {sum(nar[x] for x in prot)/G:.1f}G")
print(f"live but NOT protected (max from old gens + all project roots): {sum(nar[x] for x in count if x not in prot)/G:.1f}G")
gens_only = clos(owners["old-system-generations"]) - prot
print(f"old gens beyond protected: {sum(nar[x] for x in gens_only)/G:.1f}G")
# project roots by link age
now = time.time()
age_of = {}
for line in open(roots_file):
    m = re.match(r'^"?(.*?)"? -> (/nix/store/\S+)', line.strip())
    if not m: continue
    link, target = m.groups()
    g = group(link, target)
    try: a = (now - os.lstat(link).st_mtime) / 86400
    except OSError: a = 0
    age_of[g] = min(age_of.get(g, 1e9), a)
for cut in (30, 14, 7):
    stale = [g for g in owners if g not in prot_groups and g != "old-system-generations" and age_of.get(g, 0) >= cut]
    s = clos(set().union(*(owners[g] for g in stale))) if stale else set()
    print(f"project roots older than {cut}d: {len(stale)} groups, beyond protected: {sum(nar[x] for x in s - prot)/G:.1f}G; with old gens: {sum(nar[x] for x in (s|clos(owners['old-system-generations'])) - prot)/G:.1f}G")

print()
for cut in (30, 14, 7):
    drop = {g for g in owners if g not in prot_groups and age_of.get(g, 0) >= cut} | {"old-system-generations"}
    keep = clos(set().union(*(owners[g] for g in owners if g not in drop)))
    freed = sum(nar[x] for x in count if x not in keep)
    print(f"EXACT: drop old gens + roots >= {cut}d -> GC frees {freed/G:.1f}G live + {dead/G:.1f}G dead")
    if cut == 14:
        for g in sorted(drop, key=lambda g: -age_of.get(g, 0)):
            print(f"   {age_of.get(g,0):6.0f}d  {g}")
