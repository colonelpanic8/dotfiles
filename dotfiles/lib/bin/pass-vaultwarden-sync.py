#!/usr/bin/env python3
"""Refresh personal Vaultwarden logins from pass without deleting vault items."""

import argparse
import base64
import collections
import concurrent.futures
import copy
import datetime
import fcntl
import json
import os
import re
import shlex
import shutil
import signal
import subprocess
import sys
import tempfile
import textwrap
import threading
import uuid
from pathlib import Path

SERVER = "https://jimi-hendnix.taileb3aad.ts.net:8222"
MASTER_ENTRY = "jimi-hendnix.taileb3aad.ts.net/vaultwarden"
MARKER = "pass-sync-path"
NOTES_START = "\n\n--- pass metadata ---\n"
# AES-CBC padding and base64 framing keep 7400 UTF-8 bytes below 10000 encrypted characters.
NOTES_BYTES = 7400
BW_COMMAND = "bw"


def run(command, *, env=None, stdin=None):
    result = subprocess.run(
        command,
        input=stdin,
        env=env,
        capture_output=True,
        text=True,
        timeout=180,
        check=False,
    )
    if result.returncode:
        raise RuntimeError(f"{Path(command[0]).name} failed (secret output suppressed)")
    return result.stdout


def read_pass(name, root):
    env = dict(os.environ, PASSWORD_STORE_DIR=str(root))
    env["PASSWORD_STORE_GPG_OPTS"] = "--batch --no-tty"
    passphrase_file = os.getenv("PASS_SYNC_GPG_PASSPHRASE_FILE")
    if passphrase_file:
        passphrase_file = os.path.expandvars(passphrase_file)
        if any(c.isspace() for c in passphrase_file):
            raise RuntimeError("GPG passphrase file path must not contain whitespace")
        env["PASSWORD_STORE_GPG_OPTS"] += (
            " --pinentry-mode loopback --passphrase-file " + passphrase_file
        )
    return run(["pass", "show", name], env=env)


def source_entry(path, contents):
    lines = contents.splitlines()
    if not lines:
        raise RuntimeError(f"Empty password-store entry: {path}")
    metadata_text = "\n".join(lines[1:]).strip()
    metadata_lines = metadata_text.splitlines()
    metadata = {}
    spans = []
    index = 0
    while index < len(metadata_lines):
        start = index
        match = re.match(r"^([\w -]+):\s*(.*)$", metadata_lines[index])
        index += 1
        if match:
            key, value = match[1].lower(), match[2]
            if value in {"|", "|-", "|+"}:
                block = []
                while index < len(metadata_lines) and (
                    not metadata_lines[index] or metadata_lines[index][0].isspace()
                ):
                    block.append(metadata_lines[index])
                    index += 1
                value = textwrap.dedent("\n".join(block)) + (
                    "" if value == "|-" else "\n"
                )
            metadata[key] = value
            spans.append((start, index, key, value))
    username = next(
        (metadata[k] for k in ("user", "username", "login", "email") if k in metadata),
        None,
    )
    parent = str(Path(path).parent)
    leaf = Path(path).name
    if (
        username is None
        and parent != "."
        and "." in Path(parent).name
        and leaf not in {"unknown-login", "token", "api-key", "password"}
    ):
        username = leaf
    uris = [metadata[k] for k in ("url", "uri", "website") if metadata.get(k)]
    return {
        "path": path,
        "password": lines[0],
        "username": username,
        "uris": uris,
        "fields": metadata,
        "field_spans": spans,
        "totp": next(
            (line for line in lines[1:] if line.startswith("otpauth://")),
            metadata.get("totp") or metadata.get("otpauth"),
        ),
        "metadata": metadata_text,
    }


def legacy_names(path):
    p = Path(path)
    pairs = {(p.name, str(p.parent) if p.parent != Path(".") else "")}
    if p.parent != Path("."):
        grandparent = p.parent.parent
        pairs.add((p.parent.name, str(grandparent) if grandparent != Path(".") else ""))
    pairs.add((path, ""))
    if p.parent.parent == Path(".") and "." not in p.parent.name:
        pairs.add((p.name, ""))
    return pairs


def normalized_name(name):
    name = re.sub(r"\.(com|net|org|io|ai|co|dev)$", "", name.lower())
    return re.sub(r"[^a-z0-9]", "", name)


def match_entries(sources, items, folders):
    personal = [i for i in items if not i.get("organizationId")]
    by_marker = collections.defaultdict(list)
    by_name = collections.defaultdict(list)
    by_alias = collections.defaultdict(list)
    by_password = collections.defaultdict(list)
    source_passwords = collections.Counter(s["password"] for s in sources)
    for item in personal:
        markers = [f["value"] for f in item.get("fields") or [] if f["name"] == MARKER]
        for marker in markers:
            by_marker[marker].append(item)
        if not markers:
            by_name[(item["name"], folders.get(item.get("folderId"), ""))].append(item)
            by_alias[normalized_name(item["name"])].append(item)
            if (item.get("login") or {}).get("password"):
                by_password[item["login"]["password"]].append(item)
    proposals = []
    problems = []
    for source in sources:
        path = source["path"]
        candidates = by_marker[path]
        if candidates:
            if len(candidates) != 1:
                raise RuntimeError(f"Duplicate sync markers for {path}")
            scored = [(1000, candidates[0])]
        else:
            names = legacy_names(path)
            candidates = list(
                {
                    i["id"]: i
                    for i in [
                        *(
                            i
                            for name, folder in names
                            for i in by_alias[normalized_name(name)]
                            if normalized_name(folders.get(i.get("folderId"), ""))
                            == normalized_name(folder)
                        ),
                        *(i for name in names for i in by_name[name]),
                    ]
                }.values()
            )
            scored = []
            p = Path(path)
            for item in candidates:
                login = item.get("login") or {}
                direct = item["name"] in {path, p.name}
                if direct:
                    score = 100
                elif item["name"] == p.parent.name:
                    score = 40
                elif normalized_name(item["name"]) == normalized_name(p.parent.name):
                    score = 30
                else:
                    score = 20
                folder = folders.get(item.get("folderId"), "")
                if folder and normalized_name(folder) == normalized_name(str(p.parent)):
                    score += 100
                score += 20 * bool(
                    source["username"] and login.get("username") == source["username"]
                )
                score += 30 * bool(
                    source["password"] and login.get("password") == source["password"]
                )
                score += 10 * (p.name == "unknown-login")
                scored.append((score, item))
            if (
                not scored
                and len(source["password"]) >= 12
                and source_passwords[source["password"]] == 1
            ):
                same_password = by_password[source["password"]]
                if len(same_password) == 1:
                    old_username = (same_password[0].get("login") or {}).get("username")
                    if (
                        not source["username"]
                        or not old_username
                        or source["username"].casefold() == old_username.casefold()
                    ):
                        scored = [(10, same_password[0])]
        if scored:
            best = max(score for score, _ in scored)
            winners = [item for score, item in scored if score == best]
            if len(winners) != 1:
                descriptions = ", ".join(
                    f"{folders.get(i.get('folderId'), '')}/{i['name']} ({i['id']})"
                    for i in winners
                )
                problems.append(
                    f"Ambiguous existing vault items for {path}: {descriptions}"
                )
                continue
            proposals.append((best, path, winners[0]))
    matches = {}
    claimed = {}
    for score, path, item in sorted(proposals, key=lambda p: (-p[0], p[1])):
        previous = claimed.get(item["id"])
        if previous:
            if previous[0] == score:
                problems.append(
                    f"{path} and {previous[1]} match one vault item equally"
                )
        else:
            if item["type"] != 1:
                raise RuntimeError(f"Existing item for {path} is not a login")
            claimed[item["id"]] = (score, path)
            matches[path] = item
    if problems:
        raise RuntimeError("No changes made:\n" + "\n".join(problems))
    return matches


def desired_item(source, existing=None):
    item = (
        copy.deepcopy(existing)
        if existing
        else {
            "type": 1,
            "name": source["path"],
            "login": {},
            "fields": [],
            "notes": None,
            "favorite": False,
            "reprompt": 0,
        }
    )
    login = item.setdefault("login", {})
    if source["password"]:
        login["password"] = source["password"]
    else:
        login.pop("password", None)
    if source["username"] is not None:
        if source["username"]:
            login["username"] = source["username"]
        else:
            login.pop("username", None)
    uris = login.get("uris") or []
    for uri in source["uris"]:
        if not any(u["uri"] == uri for u in uris):
            uris.append({"uri": uri, "match": None})
    if uris:
        login["uris"] = uris
    if source["totp"]:
        login["totp"] = source["totp"]
    fields = [f for f in item.get("fields") or [] if f["name"] != MARKER]
    for name, value in source["fields"].items():
        if name in {
            "user",
            "username",
            "login",
            "email",
            "url",
            "uri",
            "website",
            "totp",
            "otpauth",
        }:
            continue
        existing_field = next((f for f in fields if f["name"].lower() == name), None)
        if existing_field is None:
            fields.append({"name": name, "value": value, "type": 1})
        else:
            existing_field["value"] = value
    fields.append({"name": MARKER, "value": source["path"], "type": 0})
    item["fields"] = fields
    notes = (item.get("notes") or "").split(NOTES_START, 1)[0]
    metadata_lines = source["metadata"].splitlines()
    structured = [
        (sum(len(line) for line in metadata_lines[start:end]), start, end)
        for start, end, key, value in source["field_spans"]
        if source["fields"].get(key) == value
    ]
    for _, start, end in sorted(structured, reverse=True):
        if (
            len(
                (
                    notes
                    + NOTES_START
                    + "\n".join(line for line in metadata_lines if line is not None)
                ).encode("utf-8")
            )
            <= NOTES_BYTES
        ):
            break
        metadata_lines[start:end] = [None] * (end - start)
    metadata = "\n".join(line for line in metadata_lines if line is not None).strip()
    if (
        len((notes + (NOTES_START + metadata if metadata else "")).encode("utf-8"))
        > NOTES_BYTES
    ):
        raise RuntimeError(
            f"Notes for {source['path']} exceed the CLI's encrypted-size budget"
        )
    if metadata:
        item["notes"] = notes + NOTES_START + metadata
    elif NOTES_START in (item.get("notes") or ""):
        item["notes"] = notes or None
    return item


class Vault:
    def __init__(self, command, directory):
        self.command = command
        self.env = dict(os.environ, BITWARDENCLI_APPDATA_DIR=directory)
        for name in ("BW_SESSION", "BW_CLIENTID", "BW_CLIENTSECRET"):
            self.env.pop(name, None)

    def call(self, *args, payload=None):
        stdin = None
        if payload is not None:
            stdin = base64.b64encode(json.dumps(payload).encode()).decode()
        return run([*self.command, *args], env=self.env, stdin=stdin)

    def login(self, server, email, password):
        identifier = uuid.uuid5(
            uuid.NAMESPACE_URL, f"pass-vaultwarden-sync:{server}:{email}:{Path.home()}"
        )
        Path(self.env["BITWARDENCLI_APPDATA_DIR"], "data.json").write_text(
            json.dumps({"global_applicationId_appId": str(identifier)})
        )
        self.call("config", "server", server)
        self.env["PASS_SYNC_MASTER_PASSWORD"] = password
        try:
            self.env["BW_SESSION"] = self.call(
                "login",
                email,
                "--passwordenv",
                "PASS_SYNC_MASTER_PASSWORD",
                "--raw",
                "--nointeraction",
            ).strip()
        finally:
            self.env.pop("PASS_SYNC_MASTER_PASSWORD", None)

    def items(self):
        self.call("sync")
        return json.loads(self.call("list", "items"))


def apply_changes(vault, changes, jobs, runtime):
    completed = 0
    progress_lock = threading.Lock()

    def apply_partition(partition):
        nonlocal completed
        with tempfile.TemporaryDirectory(
            prefix="pass-vaultwarden-worker-", dir=runtime
        ) as directory:
            shutil.copytree(
                vault.env["BITWARDENCLI_APPDATA_DIR"], directory, dirs_exist_ok=True
            )
            worker = Vault(vault.command, directory)
            worker.env = dict(vault.env, BITWARDENCLI_APPDATA_DIR=directory)
            for path, old, new in partition:
                try:
                    if old:
                        worker.call("edit", "item", old["id"], payload=new)
                    else:
                        worker.call("create", "item", payload=new)
                except RuntimeError as error:
                    raise RuntimeError(f"Failed to sync {path}: {error}") from None
                with progress_lock:
                    completed += 1
                    if completed % 25 == 0:
                        print(f"Applied {completed}/{len(changes)} changes", flush=True)

    partitions = [changes[i::jobs] for i in range(min(jobs, len(changes)))]
    with concurrent.futures.ThreadPoolExecutor(max_workers=jobs) as executor:
        futures = [
            executor.submit(apply_partition, partition) for partition in partitions
        ]
        for future in concurrent.futures.as_completed(futures):
            future.result()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--apply", action="store_true", help="write changes (default: preview)"
    )
    parser.add_argument("--server", default=SERVER)
    parser.add_argument("--email", default="ivanmalison@gmail.com")
    parser.add_argument("--master-entry", default=MASTER_ENTRY)
    parser.add_argument(
        "--store",
        type=Path,
        default=Path(os.getenv("PASSWORD_STORE_DIR", Path.home() / ".password-store")),
    )
    parser.add_argument("--bw-command", default=BW_COMMAND)
    parser.add_argument("--jobs", type=int, choices=range(1, 9), default=4)
    parser.add_argument(
        "--audit-sizes",
        action="store_true",
        help="report large records without printing values",
    )
    parser.add_argument(
        "--only", action="append", help="sync only this pass entry (repeatable)"
    )
    args = parser.parse_args()
    os.umask(0o077)
    runtime = Path(os.getenv("XDG_RUNTIME_DIR", f"/run/user/{os.getuid()}"))
    with (runtime / "pass-vaultwarden-sync.lock").open("w") as lock:
        try:
            fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
        except BlockingIOError:
            print("Another pass-vaultwarden-sync is running; skipping this invocation.")
            return
        paths = sorted(
            p.relative_to(args.store).as_posix()[:-4]
            for p in args.store.rglob("*.gpg")
            if not p.is_symlink()
            and ".git" not in p.relative_to(args.store).parts
            and not p.stem.endswith("-tmp")
        )
        if not paths:
            raise RuntimeError("No source entries found")
        if args.only:
            missing = set(args.only) - set(paths)
            if missing:
                raise RuntimeError(
                    "Unknown source entries: " + ", ".join(sorted(missing))
                )
            paths = [p for p in paths if p in args.only]
        sources = [source_entry(p, read_pass(p, args.store)) for p in paths]
        password = read_pass(args.master_entry, args.store).splitlines()[0]
        with tempfile.TemporaryDirectory(
            prefix="pass-vaultwarden-", dir=runtime
        ) as directory:
            command = shlex.split(args.bw_command)
            vault = Vault(command, directory)
            vault.login(args.server, args.email, password)
            try:
                items = vault.items()
                folders = {
                    f["id"]: f["name"]
                    for f in json.loads(vault.call("list", "folders"))
                }
                matches = match_entries(sources, items, folders)
                if args.audit_sizes:
                    for source in sources:
                        old_notes = len(
                            (matches.get(source["path"]) or {}).get("notes") or ""
                        )
                        largest_field = max(
                            (len(value) for value in source["fields"].values()),
                            default=0,
                        )
                        if (
                            len(source["metadata"]) + old_notes > 8000
                            or largest_field > 8000
                        ):
                            print(
                                "source",
                                source["path"],
                                "metadata chars",
                                len(source["metadata"]),
                                "old notes chars",
                                old_notes,
                                "largest field chars",
                                largest_field,
                            )
                    for item in items:
                        if (
                            not item.get("organizationId")
                            and len(item.get("notes") or "") > 8000
                        ):
                            print(
                                "vault", item["name"], "notes chars", len(item["notes"])
                            )
                    return
                changes = []
                for source in sources:
                    old = matches.get(source["path"])
                    new = desired_item(source, old)
                    if new != old:
                        changes.append((source["path"], old, new))
                created = sum(old is None for _, old, _ in changes)
                print(
                    f"Source: {len(sources)}; create: {created}; update: {len(changes) - created}; unchanged: {len(sources) - len(changes)}",
                    flush=True,
                )
                if not args.apply:
                    for path, old, _ in changes:
                        print(f"{'update' if old else 'create'}: {path}")
                    print("Preview only. Run with --apply to sync.")
                    return
                if changes:
                    backup = Path.home() / ".local/state/pass-vaultwarden-sync/backups"
                    backup.mkdir(parents=True, exist_ok=True)
                    filename = backup / (
                        datetime.datetime.now(datetime.timezone.utc).strftime(
                            "%Y%m%dT%H%M%S%fZ"
                        )
                        + ".json"
                    )
                    encrypted = vault.call(
                        "export", "--format", "encrypted_json", "--raw"
                    )
                    if not json.loads(encrypted).get("encrypted"):
                        raise RuntimeError("Vault export is not encrypted")
                    filename.write_text(encrypted)
                    print(f"Encrypted personal-vault backup: {filename}", flush=True)
                apply_changes(vault, changes, args.jobs, runtime)
                actual = vault.items()
                verified = match_entries(sources, actual, folders)
                for source in sources:
                    item = verified.get(source["path"])
                    if item is None or desired_item(source, item) != item:
                        raise RuntimeError(f"Verification failed for {source['path']}")
                print(
                    f"Verified all {len(sources)} entries against production; repeat sync would make no changes.",
                    flush=True,
                )
            finally:
                vault.call("logout")


if __name__ == "__main__":
    signal.signal(signal.SIGTERM, lambda signum, _frame: sys.exit(128 + signum))
    try:
        main()
    except (RuntimeError, subprocess.TimeoutExpired, OSError) as error:
        print(f"pass-vaultwarden-sync: {error}", file=sys.stderr)
        sys.exit(1)
