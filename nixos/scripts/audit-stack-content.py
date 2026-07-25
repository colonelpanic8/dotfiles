#!/usr/bin/env python3
"""Verify every manifest entry's content actually reached the integration branch.

The tree-vs-baseline diff used during the applyPatches migration was the WRONG
oracle: the baseline was itself defective (it under-carried #4477, and encoded a
different design than the branch for #4505 and show-remote-host-name). Anything
resolved by copying from baseline silently inherited those defects.

This checks the right thing instead: for each entry, every substantive line the
BRANCH adds relative to upstream main should be present in the built tree.

Non-zero MISSING is not automatically a bug -- a later entry may legitimately
have rewritten those lines. But every one needs an explanation, and a large
count on an entry whose conflicts were resolved by copying from a reference tree
is a strong signal that the entry's content was dropped.

Run after every rebuild, before switching.
"""
import subprocess, tomllib, sys
REPO="/home/imalison/Projects/t3code"
STACK="e181533871fc27f9e6a7bb0fa3e7c41850ba2a9c"
def sh(*a):
    return subprocess.run(["git","-C",REPO,*a],capture_output=True,text=True).stdout
main = sh("rev-parse","origin/main").strip()

entries=[]
for m in ["/srv/dotfiles/nix-shared/t3code-stack.toml",
          "/srv/dotfiles/nix-shared/t3code-thread-picker.toml"]:
    d=tomllib.load(open(m,"rb"))
    for e in d["entry"]:
        if e.get("pin"): entries.append((e.get("pr") or e.get("branch"), e["pin"]))

def significant(line):
    s=line[1:].strip()
    return len(s)>12 and not s.startswith(("//","/*","*","import ","}",")","],"))

print(f"{'entry':<34} {'files':>5} {'addlines':>8} {'MISSING':>8}")
bad=[]
for label,pin in entries:
    oid=sh("rev-parse",f"{pin}^{{commit}}").strip()
    if not oid: print(f"{label:<34}  UNRESOLVED"); continue
    files=[f for f in sh("diff","--name-only",f"{main}...{oid}").split() if f.endswith((".ts",".tsx"))]
    tot=miss=0
    worst={}
    for f in files:
        added=[l for l in sh("diff","-U0",f"{main}...{oid}","--",f).splitlines()
               if l.startswith("+") and not l.startswith("+++") and significant(l)]
        if not added: continue
        cur=sh("show",f"{STACK}:{f}")
        if not cur: 
            miss+=len(added); tot+=len(added); worst[f]=len(added); continue
        m2=[l for l in added if l[1:].strip() not in cur]
        tot+=len(added); miss+=len(m2)
        if m2: worst[f]=len(m2)
    flag="  <-- LOSS" if miss>0 else ""
    print(f"{label:<34} {len(files):>5} {tot:>8} {miss:>8}{flag}")
    if miss: bad.append((label,miss,worst))

print("\n=== detail for entries with missing lines ===")
for label,miss,worst in sorted(bad,key=lambda x:-x[1]):
    print(f"\n{label}: {miss} added lines absent from the built tree")
    for f,n in sorted(worst.items(),key=lambda x:-x[1])[:4]:
        print(f"    {n:>4}  {f}")
