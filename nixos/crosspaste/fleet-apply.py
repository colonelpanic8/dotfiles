"""Make CrossPaste trust every member of the fleet file without PIN pairing.

A pairing in CrossPaste is the peer's ECDH public key in CryptPublicKey plus a
SyncRuntimeInfoEntity row telling the resolver where to reach it. This writes
both for every fleet member, and can install this device's own identity.
"""

import argparse
import base64
import json
import os
import sqlite3
import time
from pathlib import Path

compact = {"separators": (",", ":")}


def load_fleet(path):
    fleet = json.loads(Path(path).read_text())
    if fleet.get("version") != 1:
        raise SystemExit(f"Unsupported fleet version: {fleet.get('version')}")
    return fleet["members"]


def write_if_changed(path, data):
    if path.exists() and path.read_bytes() == data:
        return False
    path.parent.mkdir(parents=True, exist_ok=True)
    tmp = path.with_name(path.name + ".tmp")
    tmp.write_bytes(data)
    tmp.chmod(0o600)
    os.replace(tmp, path)
    return True


def install_identity(identity_path, data_dir):
    identity = json.loads(Path(identity_path).read_text())
    metadata = json.dumps({"appInstanceId": identity["appInstanceId"]}, **compact).encode()
    changed = write_if_changed(data_dir / ".metadata", metadata)
    changed |= write_if_changed(
        data_dir / "encrypt" / "secure.data", base64.b64decode(identity["secureData"])
    )
    if changed:
        print(f"Installed fleet identity {identity['appInstanceId']}")
    return identity["appInstanceId"]


def own_instance_id(data_dir):
    try:
        return json.loads((data_dir / ".metadata").read_text())["appInstanceId"]
    except (OSError, ValueError, KeyError):
        return None


def host_info(address):
    return {"networkPrefixLength": 32, "hostAddress": address, "lastSeen": 0}


def seed(db_path, members, own_id):
    if not db_path.exists():
        return
    now = int(time.time() * 1000)
    with sqlite3.connect(db_path, timeout=30) as db:
        tables = {row[0] for row in db.execute("SELECT name FROM sqlite_master")}
        if not {"CryptPublicKey", "SyncRuntimeInfoEntity"} <= tables:
            return
        for member in members:
            peer = member["appInstanceId"]
            if peer == own_id:
                continue
            key = base64.b64decode(member["cryptPublicKey"])
            stored = db.execute(
                "SELECT serialized FROM CryptPublicKey WHERE appInstanceId = ?", (peer,)
            ).fetchone()
            if stored is None or bytes(stored[0]) != key:
                db.execute(
                    "INSERT OR REPLACE INTO CryptPublicKey (appInstanceId, serialized) VALUES (?, ?)",
                    (peer, key),
                )
                print(f"Trusted {member['name']} ({peer})")

            addresses = member.get("hostAddresses", [])
            row = db.execute(
                "SELECT hostInfo FROM SyncRuntimeInfoEntity WHERE appInstanceId = ?", (peer,)
            ).fetchone()
            if row is None:
                platform = member["platform"]
                db.execute(
                    """INSERT INTO SyncRuntimeInfoEntity (
                        appInstanceId, appVersion, userName, deviceId, deviceName,
                        platformName, platformArch, platformBitMode, platformVersion,
                        hostInfo, port, connectState, allowSend, allowReceive,
                        createTime, modifyTime
                    ) VALUES (?, '0', 'fleet', ?, ?, ?, ?, ?, ?, ?, ?, 2, 1, 1, ?, ?)""",
                    (
                        peer,
                        peer,
                        member["name"],
                        platform["name"],
                        platform["arch"],
                        platform["bitMode"],
                        platform["version"],
                        json.dumps([host_info(a) for a in addresses], **compact),
                        member.get("port", 13129),
                        now,
                        now,
                    ),
                )
                continue

            try:
                known = json.loads(row[0])
            except ValueError:
                known = []
            missing = [a for a in addresses if a not in {h.get("hostAddress") for h in known}]
            if missing:
                db.execute(
                    "UPDATE SyncRuntimeInfoEntity SET hostInfo = ?, modifyTime = ? WHERE appInstanceId = ?",
                    (json.dumps(known + [host_info(a) for a in missing], **compact), now, peer),
                )


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--fleet", required=True)
    parser.add_argument("--data-dir", required=True, type=Path)
    parser.add_argument("--identity", help="Install this device's identity first")
    parser.add_argument(
        "--interval", type=int, help="Keep re-applying every INTERVAL seconds"
    )
    args = parser.parse_args()

    members = load_fleet(args.fleet)
    own_id = (
        install_identity(args.identity, args.data_dir)
        if args.identity
        else own_instance_id(args.data_dir)
    )
    db_path = args.data_dir / "data" / "crosspaste.db"
    while True:
        try:
            seed(db_path, members, own_id)
        except sqlite3.Error as error:
            print(f"Could not seed {db_path}: {error}")
        if not args.interval:
            break
        time.sleep(args.interval)


if __name__ == "__main__":
    main()
