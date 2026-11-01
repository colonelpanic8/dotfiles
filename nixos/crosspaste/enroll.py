"""Add a device to the CrossPaste fleet.

NixOS hosts get an identity sealed with agenix to keys.agenixKeys, minted here
or imported over SSH so already paired devices keep trusting them. Other
devices (phones) keep their own key: pair them once with this machine, then
copy their public key from its database with --from-db.

  enroll.py HOST                  mint a new identity for a NixOS host
  enroll.py HOST --import-ssh     adopt the identity HOST's CrossPaste already has
  enroll.py NAME --from-db ID     add a paired device by its appInstanceId
"""

import argparse
import base64
import json
import socket
import sqlite3
import struct
import subprocess
import uuid
from pathlib import Path

from cryptography.hazmat.primitives import serialization
from cryptography.hazmat.primitives.asymmetric import ec

nixos = Path(__file__).resolve().parent.parent
root = nixos.parent
fleet_path = root / "nix-shared" / "crosspaste-fleet.json"
local_data = Path.home() / ".local/share/.crosspaste"
linux_platform = {"name": "Linux", "arch": "amd64", "bitMode": 64, "version": "NixOS"}


def segment(data, offset):
    (size,) = struct.unpack(">I", data[offset:offset + 4])
    return data[offset + 4:offset + 4 + size], offset + 4 + size


def frame(*parts):
    return b"".join(struct.pack(">I", len(p)) + p for p in parts)


def crypt_public_key(secure_data):
    sign, offset = segment(secure_data, 0)
    crypt, _ = segment(secure_data, offset)
    return segment(crypt, 0)[0]


def key_pair():
    key = ec.generate_private_key(ec.SECP256R1())
    public = key.public_key().public_bytes(
        serialization.Encoding.DER, serialization.PublicFormat.SubjectPublicKeyInfo
    )
    private = key.private_bytes(
        serialization.Encoding.DER,
        serialization.PrivateFormat.PKCS8,
        serialization.NoEncryption(),
    )
    return frame(public, private)


def mint():
    return uuid.uuid4().hex, frame(key_pair(), key_pair())


def import_ssh(host):
    if host == socket.gethostname():
        metadata = json.loads((local_data / ".metadata").read_text())
        return metadata["appInstanceId"], (local_data / "encrypt/secure.data").read_bytes()
    output = subprocess.run(
        [
            "ssh",
            "-o",
            "BatchMode=yes",
            host,
            "cat ~/.local/share/.crosspaste/.metadata; echo; base64 -w0 ~/.local/share/.crosspaste/encrypt/secure.data",
        ],
        check=True,
        capture_output=True,
        text=True,
    ).stdout.splitlines()
    return json.loads(output[0])["appInstanceId"], base64.b64decode(output[1])


def seal(host, app_instance_id, secure_data):
    recipients = json.loads(
        subprocess.run(
            ["nix", "eval", "--json", "-f", str(root / "nix-shared/keys.nix"), "agenixKeys"],
            check=True,
            capture_output=True,
            text=True,
        ).stdout
    )
    identity = json.dumps(
        {"appInstanceId": app_instance_id, "secureData": base64.b64encode(secure_data).decode()}
    )
    output = nixos / "secrets" / f"crosspaste-identity.{host}.age"
    args = ["age", "--encrypt", "--output", str(output)]
    for recipient in recipients:
        args += ["--recipient", recipient]
    subprocess.run(args, input=identity, text=True, check=True)
    print(f"Wrote {output.relative_to(root)}")


def tailnet_address(host):
    return subprocess.run(
        ["tailscale", "ip", "-4", host], check=True, capture_output=True, text=True
    ).stdout.split()[0]


def update_fleet(member):
    fleet = json.loads(fleet_path.read_text())
    fleet["members"] = sorted(
        [m for m in fleet["members"] if m["name"] != member["name"]] + [member],
        key=lambda m: m["name"],
    )
    fleet_path.write_text(json.dumps(fleet, indent=2) + "\n")
    print(f"Updated {fleet_path.relative_to(root)} with {member['name']}")


def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("name")
    source = parser.add_mutually_exclusive_group()
    source.add_argument("--import-ssh", action="store_true")
    source.add_argument("--from-db", metavar="APP_INSTANCE_ID")
    parser.add_argument("--address", help="Tailnet IPv4 (default: tailscale ip -4 NAME)")
    args = parser.parse_args()
    address = args.address or tailnet_address(args.name)

    if args.from_db:
        db = sqlite3.connect(f"file:{local_data / 'data/crosspaste.db'}?mode=ro", uri=True)
        key = db.execute(
            "SELECT serialized FROM CryptPublicKey WHERE appInstanceId = ?", (args.from_db,)
        ).fetchone()
        info = db.execute(
            "SELECT platformName, platformArch, platformBitMode, platformVersion, port "
            "FROM SyncRuntimeInfoEntity WHERE appInstanceId = ?",
            (args.from_db,),
        ).fetchone()
        if key is None or info is None:
            raise SystemExit(f"{args.from_db} is not paired with this machine")
        update_fleet(
            {
                "name": args.name,
                "appInstanceId": args.from_db,
                "platform": dict(zip(["name", "arch", "bitMode", "version"], info[:4])),
                "cryptPublicKey": base64.b64encode(bytes(key[0])).decode(),
                "hostAddresses": [address],
                "port": info[4],
            }
        )
        return

    app_instance_id, secure_data = import_ssh(args.name) if args.import_ssh else mint()
    seal(args.name, app_instance_id, secure_data)
    update_fleet(
        {
            "name": args.name,
            "appInstanceId": app_instance_id,
            "platform": linux_platform,
            "cryptPublicKey": base64.b64encode(crypt_public_key(secure_data)).decode(),
            "hostAddresses": [address],
            "port": 13129,
        }
    )


if __name__ == "__main__":
    main()
