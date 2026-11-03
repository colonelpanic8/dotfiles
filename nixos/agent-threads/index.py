"""Build a uniform SQLite index of agent threads from agent-thread-backup's mirror.

Usage:
  agent-threads-index build [--root DIR] [--legacy DIR]
  agent-threads-index resolve-repos NAME TARGET [--root DIR]
  agent-threads-index repos [--root DIR]

`build` parses every host's mirror into ROOT/index.sqlite. `resolve-repos`
asks a host's git which repository each recorded cwd belongs to and appends
the answers to ROOT/NAME/repo-map.jsonl, so they survive the worktree being
deleted. `repos` recomputes the repo columns without reparsing transcripts.
"""

import argparse
import datetime
import glob
import json
import os
import re
import socket
import sqlite3
import subprocess
import sys

SCHEMA = """
CREATE TABLE threads (
  key TEXT PRIMARY KEY,
  host TEXT,
  app TEXT,          -- t3code | paseo | claude-code | codex | opencode
  provider TEXT,     -- claude | codex | opencode
  account TEXT,      -- provider home, e.g. .claude-dean
  native_id TEXT,
  parent_key TEXT,
  origin TEXT,       -- human | agent | system
  title TEXT,
  cwd TEXT,
  repo_root TEXT,
  repo TEXT,         -- normalized remote (github.com/owner/repo) or repo root
  branch TEXT,
  model TEXT,
  client TEXT,      -- claude entrypoint, or codex originator/source
  started_at TEXT,
  last_at TEXT,
  has_transcript INTEGER,
  human_messages INTEGER,
  agent_messages INTEGER,  -- user-role messages another agent sent
  assistant_messages INTEGER,
  source TEXT
);
CREATE TABLE messages (
  id INTEGER PRIMARY KEY,
  thread_key TEXT,
  ts TEXT,
  role TEXT,         -- user | assistant
  author TEXT,       -- human | agent | system
  text TEXT
);
CREATE TABLE prompt_history (
  host TEXT,
  account TEXT,
  ts TEXT,
  session_id TEXT,
  project TEXT,
  text TEXT
);
CREATE TABLE repo_map (
  host TEXT,
  cwd TEXT,
  repo_root TEXT,
  remote TEXT,
  PRIMARY KEY (host, cwd)
);
CREATE INDEX messages_thread ON messages(thread_key, author);
CREATE INDEX messages_author ON messages(author, ts);
CREATE INDEX threads_origin ON threads(origin, started_at);
CREATE INDEX threads_repo ON threads(repo);
"""

# User-role text that the harness, not a person, put into the conversation.
INJECTED = re.compile(
    r"^\s*(<environment_context|<user_instructions|<permissions instructions|"
    r"# AGENTS\.md instructions|<INSTRUCTIONS>|<paseo-system|<task-notification|"
    r"<system-reminder|<subagent_notification|<turn_aborted|<local-command-|"
    r"Base directory for this skill|\[Request interrupted|Caveat: The messages below|"
    r"Note: the T3 server restarted|\[Image[: #]|This session is being continued|"
    r"<collaboration_mode|<skill>|<user_shell_command|<recommended_plugins|<goal_context|"
    r"<codex_internal_context|<codex_delegation|<realtime_delegation|<heartbeat|"
    r"<chat-history-summary|<in-app-browser-context|# In app browser:|"
    r"Warning: apply_patch was requested)"
)
# Prompts that mark a whole session as an app's internal helper call.
UTILITY = re.compile(
    r"^\s*(Generate a title|\[structured-output-enforce\]|You are generating|"
    r"Summarize (this|the) (conversation|agent)|Write a (short|concise) title|"
    r"Update the one-line purpose summary)",
    re.I,
)
UUID = re.compile(r"[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}")


def iso_ms(ms):
    if ms is None:
        return None
    return datetime.datetime.fromtimestamp(ms / 1000, datetime.timezone.utc).strftime("%Y-%m-%dT%H:%M:%S.%f")[:-3] + "Z"


def read_lines(path):
    if path.endswith(".zst"):
        data = subprocess.run(["zstd", "-dc", path], capture_output=True, check=True).stdout
        return data.decode("utf-8", "replace").splitlines()
    with open(path, encoding="utf-8", errors="replace") as f:
        return f.readlines()


def records(path):
    for line in read_lines(path):
        try:
            yield json.loads(line)
        except ValueError:
            continue


def classify(text):
    if INJECTED.match(text):
        return "system"
    return "human"


class Thread(dict):
    def __init__(self, **kw):
        super().__init__(
            key=None, host=None, app=None, provider=None, account=None,
            native_id=None, parent_key=None, origin="human", title=None,
            cwd=None, branch=None, model=None, client=None, started_at=None, last_at=None,
            has_transcript=0, human_messages=None, agent_messages=None,
            assistant_messages=None, source=None, remote=None,
        )
        self.update(kw)
        self.messages = []

    def add(self, ts, role, author, text):
        text = (text or "").strip()
        if not text:
            return
        self.messages.append((ts, role, author, text))
        if ts:
            if not self["started_at"] or ts < self["started_at"]:
                self["started_at"] = ts
            if not self["last_at"] or ts > self["last_at"]:
                self["last_at"] = ts

    def adopt(self, native):
        """Take messages and missing metadata from a parsed native transcript."""
        self.messages = native.messages
        self["has_transcript"] = 1
        for k in ("cwd", "branch", "model", "started_at", "last_at", "remote"):
            if not self[k]:
                self[k] = native[k]
        if self["origin"] == "agent":
            self.messages = [
                (ts, role, "agent" if author == "human" else author, text)
                for ts, role, author, text in self.messages
            ]

    def classify_standalone(self):
        """Decide who started a session that no app recorded."""
        first = next((m for m in self.messages if m[1] == "user" and m[2] != "system"), None)
        if self["origin"] == "agent" or self["client"] == "sdk-cli":
            # Subagents, `codex exec`, and `claude -p` are run by tools and agents.
            self["origin"] = "agent"
            author = "agent"
        elif first is None or UTILITY.match(first[3]):
            self["origin"] = "system"
            author = "system"
        else:
            return
        self.messages = [(ts, r, author if r == "user" and a == "human" else a, t) for ts, r, a, t in self.messages]


def parse_claude(path):
    t = Thread(provider="claude", native_id=os.path.basename(path).split(".")[0], source=path, has_transcript=1)
    assistant = {}
    for d in records(path):
        typ = d.get("type")
        if typ in ("ai-title", "custom-title", "summary"):
            title = d.get("customTitle") or d.get("aiTitle") or d.get("title") or d.get("summary")
            if title and (typ == "custom-title" or not t["title"]):
                t["title"] = title
            continue
        if typ not in ("user", "assistant") or d.get("isSidechain"):
            continue
        t["cwd"] = t["cwd"] or d.get("cwd")
        t["branch"] = t["branch"] or d.get("gitBranch")
        t["client"] = t["client"] or d.get("entrypoint")
        ts = d.get("timestamp")
        msg = d.get("message") or {}
        content = msg.get("content")
        if typ == "assistant":
            t["model"] = t["model"] or msg.get("model")
            text = "".join(c.get("text", "") for c in content or [] if isinstance(c, dict) and c.get("type") == "text")
            if text:
                mid = msg.get("id") or d.get("uuid")
                if mid in assistant:
                    assistant[mid][3] += text
                else:
                    assistant[mid] = [ts, "assistant", "agent", text]
                    t.messages.append(assistant[mid])
            continue
        if d.get("isMeta"):
            continue
        if isinstance(content, list):
            if any(isinstance(c, dict) and c.get("type") == "tool_result" for c in content):
                continue
            content = "\n".join(c.get("text", "") for c in content if isinstance(c, dict) and c.get("type") == "text")
        if not isinstance(content, str):
            continue
        kind = (d.get("origin") or {}).get("kind")
        author = "system" if kind and kind != "human" else classify(content)
        t.add(ts, "user", author, content)
    t.messages = [tuple(m) for m in t.messages]
    for m in t.messages:
        if m[0]:
            t["started_at"] = min(filter(None, [t["started_at"], m[0]]))
            t["last_at"] = max(filter(None, [t["last_at"], m[0]]))
    return t


def parse_codex(path):
    m = UUID.search(os.path.basename(path)) or re.search(r"([0-9a-f]{8}-[0-9a-f-]{27,})", os.path.basename(path))
    t = Thread(provider="codex", native_id=m.group(0) if m else os.path.basename(path), source=path, has_transcript=1)
    for d in records(path):
        typ = d.get("type")
        p = d.get("payload") or {}
        ts = d.get("timestamp")
        if typ == "session_meta":
            t["native_id"] = p.get("id") or t["native_id"]
            t["cwd"] = p.get("cwd")
            git = p.get("git") or {}
            t["branch"] = git.get("branch")
            t["remote"] = git.get("repository_url")
            src = p.get("source")
            t["client"] = " / ".join(filter(None, [p.get("originator"), src if isinstance(src, str) else "subagent"]))
            if isinstance(src, dict) and "subagent" in json.dumps(src).lower():
                t["origin"] = "agent"
                parent = json.dumps(src)
                pm = UUID.search(parent) or re.search(r"[0-9a-f]{8}-[0-9a-f-]{27,}", parent)
                if pm:
                    t["parent_native"] = pm.group(0)
            elif src in ("exec", "mcp"):
                t["origin"] = "agent"
        elif typ == "turn_context":
            t["model"] = t["model"] or p.get("model")
        elif typ == "response_item" and p.get("type") == "message":
            role = p.get("role")
            parts = p.get("content") or []
            text = "".join(c.get("text", "") for c in parts if isinstance(c, dict) and c.get("type") in ("input_text", "output_text"))
            if role == "user":
                t.add(ts, "user", classify(text), text)
            elif role == "assistant":
                t.add(ts, "assistant", "agent", text)
    return t


def parse_opencode(db_path, host):
    db = sqlite3.connect(f"file:{db_path}?mode=ro", uri=True)
    texts = {}
    for mid, data in db.execute("select message_id, data from part order by time_created"):
        try:
            part = json.loads(data)
        except ValueError:
            continue
        if part.get("type") == "text" and not part.get("synthetic"):
            texts.setdefault(mid, []).append(part.get("text", ""))
    out = {}
    for sid, parent, directory, title, model, created, updated in db.execute(
        "select id, parent_id, directory, title, model, time_created, time_updated from session"
    ):
        t = Thread(host=host, provider="opencode", native_id=sid, title=title, cwd=directory, source=db_path, has_transcript=1)
        if parent:
            t["origin"] = "agent"
            t["parent_native"] = parent
        try:
            t["model"] = json.loads(model or "{}").get("id")
        except ValueError:
            pass
        out[sid] = t
    for mid, sid, created, data in db.execute("select id, session_id, time_created, data from message order by time_created"):
        t = out.get(sid)
        if not t:
            continue
        role = json.loads(data).get("role")
        text = "\n".join(texts.get(mid, []))
        if role == "user":
            t.add(iso_ms(created), "user", "agent" if t["origin"] == "agent" else classify(text), text)
        elif role == "assistant":
            t.add(iso_ms(created), "assistant", "agent", text)
    db.close()
    return out


def account_of(rel):
    return rel.split("/", 1)[0]


class Host:
    def __init__(self, root, name, legacy):
        self.name = name
        self.dir = os.path.join(root, name)
        self.home = os.path.join(self.dir, "home")
        self.legacy = os.path.join(legacy, "raw", name) if legacy else None
        self.natives = {}  # native_id -> path
        self.claims = set()

    def scan_natives(self):
        for path in glob.glob(f"{self.home}/.claude*/projects/*/*.jsonl"):
            self.natives.setdefault(os.path.basename(path)[:-6], ("claude", path))
        for path in glob.glob(f"{self.home}/.codex*/*sessions/**/rollout-*.jsonl*", recursive=True):
            m = UUID.search(os.path.basename(path)) or re.search(r"[0-9a-f]{8}-[0-9a-f-]{27,}", os.path.basename(path))
            if m:
                prev = self.natives.get(m.group(0))
                # Prefer the uncompressed rollout if Codex left both.
                if not prev or prev[1].endswith(".zst"):
                    self.natives[m.group(0)] = ("codex", path)

    def parse_native(self, sid, provider_hint=None):
        hit = self.natives.get(sid)
        if hit:
            kind, path = hit
        elif self.legacy and os.path.exists(f"{self.legacy}/transcripts/{sid}.jsonl.zst"):
            path = f"{self.legacy}/transcripts/{sid}.jsonl.zst"
            kind = provider_hint
        else:
            return None
        t = parse_claude(path) if kind == "claude" else parse_codex(path) if kind == "codex" else None
        if t is not None and hit:
            t["account"] = account_of(os.path.relpath(path, self.home))
        return t


def load_t3(host, threads):
    path = os.path.join(host.dir, "databases", "latest", "t3-statev2.sqlite")
    if not os.path.exists(path):
        return
    db = sqlite3.connect(f"file:{path}?mode=ro", uri=True)
    projects = {pid: root for pid, root in db.execute("select project_id, workspace_root from projection_projects")}
    for (payload,) in db.execute("select payload_json from orchestration_v2_projection_provider_threads"):
        ref = (json.loads(payload).get("nativeThreadRef") or {}).get("nativeId")
        if ref:
            host.claims.add(ref)
    for (payload,) in db.execute("select payload_json from orchestration_v2_projection_threads"):
        p = json.loads(payload)
        if p.get("deletedAt"):
            continue
        parent = (p.get("lineage") or {}).get("parentThreadId")
        t = Thread(
            key=f"t3:{host.name}:{p['id']}", host=host.name, app="t3code",
            provider=p.get("providerInstanceId"), native_id=p["id"],
            parent_key=f"t3:{host.name}:{parent}" if parent else None,
            origin="human" if p.get("createdBy") == "user" else "agent",
            title=p.get("title"), cwd=p.get("worktreePath") or projects.get(p.get("projectId")),
            branch=p.get("branch"), model=(p.get("modelSelection") or {}).get("model"),
            started_at=p.get("createdAt"), has_transcript=1, source=path,
        )
        threads[t["key"]] = t
    for tid, role, payload in db.execute(
        "select thread_id, role, payload_json from orchestration_v2_projection_messages order by created_at"
    ):
        t = threads.get(f"t3:{host.name}:{tid}")
        if not t:
            continue
        p = json.loads(payload)
        text = p.get("text") or ""
        if role == "user":
            author = "human" if p.get("createdBy") == "user" else "agent"
            if author == "human" and classify(text) == "system":
                author = "system"
        else:
            author = "agent"
        t.add(p.get("createdAt"), role, author, text)
    db.close()


def load_paseo(host, threads):
    records_by_id = {}
    for path in glob.glob(f"{host.home}/.paseo/agents/*/*.json"):
        try:
            d = json.load(open(path))
        except ValueError:
            continue
        records_by_id[d["id"]] = (d, path)
    if host.legacy and os.path.exists(f"{host.legacy}/manifest.json"):
        for a in json.load(open(f"{host.legacy}/manifest.json")).get("agents", []):
            if a["agentId"] in records_by_id:
                continue
            path = os.path.join(host.legacy, a["record"])
            try:
                records_by_id[a["agentId"]] = (json.load(open(path)), path)
            except (OSError, ValueError):
                continue
    for aid, (d, path) in records_by_id.items():
        parent = (d.get("labels") or {}).get("paseo.parent-agent-id")
        sid = (d.get("persistence") or {}).get("sessionId") or (d.get("runtimeInfo") or {}).get("sessionId")
        provider = d.get("provider") or ""
        base = "claude" if provider.startswith("claude") else "codex" if provider.startswith("codex") else provider
        t = Thread(
            key=f"paseo:{host.name}:{aid}", host=host.name, app="paseo", provider=base,
            account=provider, native_id=sid,
            parent_key=f"paseo:{host.name}:{parent}" if parent else None,
            origin="agent" if parent else "human", title=d.get("title"), cwd=d.get("cwd"),
            model=(d.get("config") or {}).get("model"), started_at=d.get("createdAt"),
            last_at=d.get("lastActivityAt") or d.get("updatedAt"), source=path,
        )
        if sid:
            host.claims.add(sid)
            native = host.parse_native(sid, base) if base in ("claude", "codex") else None
            if native:
                t.adopt(native)
        threads[t["key"]] = t


def load_standalone(host, threads):
    for sid, (kind, path) in host.natives.items():
        if sid in host.claims:
            continue
        t = parse_claude(path) if kind == "claude" else parse_codex(path)
        t["key"] = f"{kind}:{host.name}:{sid}"
        t["host"] = host.name
        t["app"] = "claude-code" if kind == "claude" else "codex"
        t["account"] = account_of(os.path.relpath(path, host.home))
        if t.get("parent_native"):
            t["parent_key"] = f"{kind}:{host.name}:{t['parent_native']}"
        t.classify_standalone()
        threads[t["key"]] = t
    oc = os.path.join(host.dir, "databases", "latest", "opencode.sqlite")
    if os.path.exists(oc):
        for sid, t in parse_opencode(oc, host.name).items():
            if sid in host.claims:
                continue
            t["key"] = f"opencode:{host.name}:{sid}"
            t["app"] = "opencode"
            t["account"] = ".local/share/opencode"
            if t.get("parent_native"):
                t["parent_key"] = f"opencode:{host.name}:{t['parent_native']}"
            threads[t["key"]] = t


def load_history(host):
    rows = []
    for path in glob.glob(f"{host.home}/.claude*/history.jsonl"):
        acct = account_of(os.path.relpath(path, host.home))
        for d in records(path):
            rows.append((host.name, acct, iso_ms(d.get("timestamp")), d.get("sessionId"), d.get("project"), d.get("display")))
    for path in glob.glob(f"{host.home}/.codex*/history.jsonl"):
        acct = account_of(os.path.relpath(path, host.home))
        for d in records(path):
            ts = d.get("ts")
            rows.append((host.name, acct, iso_ms(ts * 1000) if ts else None, d.get("session_id"), None, d.get("text")))
    return rows


def normalize_remote(url):
    if not url:
        return None
    url = url.strip()
    if url.startswith("remote:"):
        return url[len("remote:"):]
    m = re.match(r"^(?:[a-z+]+://)?(?:[^@/]+@)?([^/:]+)(?::\d+)?[:/](.+?)(?:\.git)?/?$", url)
    return f"{m.group(1)}/{m.group(2)}" if m else url


def known_roots(root, host):
    """Map repo roots and cwds to (repo_root, remote) from everything recorded offline."""
    cwd_root, root_remote = {}, {}
    home = os.path.join(root, host, "home")
    for path in [f"{home}/.paseo/projects/projects.json"]:
        try:
            for p in json.load(open(path)):
                if p.get("rootPath") and p.get("kind") == "git":
                    root_remote.setdefault(p["rootPath"], normalize_remote(p.get("projectKey")) if (p.get("projectKey") or "").startswith("remote:") else None)
        except (OSError, ValueError):
            pass
    try:
        for w in json.load(open(f"{home}/.paseo/projects/workspaces.json")):
            if w.get("cwd") and w.get("mainRepoRoot"):
                cwd_root[w["cwd"]] = w["mainRepoRoot"]
    except (OSError, ValueError):
        pass
    try:
        with open(os.path.join(root, host, "repo-map.jsonl")) as f:
            for line in f:
                e = json.loads(line)
                if e.get("root"):
                    cwd_root[e["cwd"]] = e["root"]
                    if e.get("remote"):
                        root_remote[e["root"]] = normalize_remote(e["remote"])
                    else:
                        root_remote.setdefault(e["root"], None)
    except OSError:
        pass
    return cwd_root, root_remote


def assign_repos(db, root):
    hosts = [h for (h,) in db.execute("select distinct host from threads")]
    db.execute("delete from repo_map")
    for host in hosts:
        cwd_root, root_remote = known_roots(root, host)
        for r in cwd_root.values():
            root_remote.setdefault(r, None)
        roots = sorted(root_remote, key=len, reverse=True)
        rows = db.execute("select key, cwd, source from threads where host = ?", (host,)).fetchall()
        codex_remote = {
            cwd: r for cwd, r in db.execute(
                "select cwd, remote from native_remotes where host = ? and remote is not null", (host,)
            )
        }
        for key, cwd, _ in rows:
            if not cwd:
                continue
            repo_root = cwd_root.get(cwd)
            if not repo_root:
                wt = re.match(r"^(.*?)/\.worktrees/[^/]+", cwd)
                repo_root = wt.group(1) if wt else next((r for r in roots if cwd == r or cwd.startswith(r + "/")), None)
            if repo_root:
                repo_root = cwd_root.get(repo_root, repo_root)
            remote = root_remote.get(repo_root) if repo_root else None
            remote = remote or normalize_remote(codex_remote.get(cwd))
            db.execute(
                "update threads set repo_root = ?, repo = ? where key = ?",
                (repo_root, remote or repo_root, key),
            )
            db.execute("insert or ignore into repo_map values (?, ?, ?, ?)", (host, cwd, repo_root, remote))


def build(root, legacy):
    tmp = os.path.join(root, ".index.sqlite.tmp")
    if os.path.exists(tmp):
        os.remove(tmp)
    db = sqlite3.connect(tmp)
    db.executescript(SCHEMA)
    db.execute("create table native_remotes (host TEXT, cwd TEXT, remote TEXT)")
    for name in sorted(os.listdir(root)):
        if not os.path.isdir(os.path.join(root, name, "home")):
            continue
        host = Host(root, name, legacy)
        host.scan_natives()
        threads = {}
        load_t3(host, threads)
        load_paseo(host, threads)
        load_standalone(host, threads)
        cols = [
            "key", "host", "app", "provider", "account", "native_id", "parent_key", "origin",
            "title", "cwd", "branch", "model", "client", "started_at", "last_at", "has_transcript", "human_messages", "agent_messages",
            "assistant_messages", "source",
        ]
        for t in threads.values():
            if t["has_transcript"]:
                t["human_messages"] = sum(1 for m in t.messages if m[2] == "human")
                t["agent_messages"] = sum(1 for m in t.messages if m[1] == "user" and m[2] == "agent")
                t["assistant_messages"] = sum(1 for m in t.messages if m[1] == "assistant")
            db.execute(f"insert into threads ({','.join(cols)}) values ({','.join('?' * len(cols))})", [t[c] for c in cols])
            db.executemany(
                "insert into messages (thread_key, ts, role, author, text) values (?, ?, ?, ?, ?)",
                [(t["key"], *m) for m in t.messages],
            )
            if t.get("remote") and t["cwd"]:
                db.execute("insert into native_remotes values (?, ?, ?)", (name, t["cwd"], t["remote"]))
        db.executemany("insert into prompt_history values (?, ?, ?, ?, ?, ?)", load_history(host))
        print(f"{name}: {len(threads)} threads", file=sys.stderr)
    assign_repos(db, root)
    db.execute("create virtual table messages_fts using fts5(text, content='messages', content_rowid='id')")
    db.execute("insert into messages_fts(messages_fts) values ('rebuild')")
    db.commit()
    db.close()
    os.replace(tmp, os.path.join(root, "index.sqlite"))


RESOLVER = r"""
import json, os, subprocess, sys
out = []
for cwd in json.loads(sys.stdin.readline()):
    e = {"cwd": cwd}
    if os.path.isdir(cwd):
        def git(*a):
            r = subprocess.run(["git", "-C", cwd, *a], capture_output=True, text=True)
            return r.stdout.strip() if r.returncode == 0 else None
        common = git("rev-parse", "--path-format=absolute", "--git-common-dir")
        top = git("rev-parse", "--show-toplevel")
        if common and common.endswith("/.git"):
            e["root"] = os.path.dirname(common)
        elif top:
            e["root"] = top
        if e.get("root"):
            e["remote"] = git("config", "--get", "remote.origin.url")
    else:
        e["missing"] = True
    out.append(e)
print(json.dumps(out))
"""


def resolve_repos(root, name, target):
    index = os.path.join(root, "index.sqlite")
    if not os.path.exists(index):
        return
    db = sqlite3.connect(f"file:{index}?mode=ro", uri=True)
    cwds = {c for (c,) in db.execute("select distinct cwd from threads where host = ? and cwd is not null", (name,))}
    db.close()
    map_path = os.path.join(root, name, "repo-map.jsonl")
    seen = set()
    if os.path.exists(map_path):
        with open(map_path) as f:
            seen = {json.loads(line)["cwd"] for line in f}
    todo = sorted(cwds - seen)
    if not todo:
        return
    local = name == socket.gethostname().split(".")[0]
    script = RESOLVER.replace("json.loads(sys.stdin.readline())", repr(todo))
    cmd = ["python3", "-I", "-"] if local else [
        "ssh", "-o", "BatchMode=yes", "-o", "ConnectTimeout=15", target, "python3", "-I", "-",
    ]
    r = subprocess.run(cmd, input=script, capture_output=True, text=True, timeout=600)
    if r.returncode != 0:
        print(f"resolve-repos {name}: {r.stderr.strip()}", file=sys.stderr)
        return
    with open(map_path, "a") as f:
        for e in json.loads(r.stdout):
            f.write(json.dumps(e) + "\n")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("command", choices=["build", "resolve-repos", "repos"])
    ap.add_argument("args", nargs="*")
    ap.add_argument("--root", default=os.path.expanduser("~/Backups/agent-threads"))
    ap.add_argument("--legacy", default=os.path.expanduser("~/Backups/paseo-conversations"))
    a = ap.parse_args()
    legacy = a.legacy if os.path.isdir(a.legacy) else None
    if a.command == "build":
        build(a.root, legacy)
    elif a.command == "resolve-repos":
        resolve_repos(a.root, *a.args)
    else:
        db = sqlite3.connect(os.path.join(a.root, "index.sqlite"))
        assign_repos(db, a.root)
        db.commit()
        db.close()


if __name__ == "__main__":
    main()
