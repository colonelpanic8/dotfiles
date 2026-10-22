"""Explicit, encrypted enrollment for personal installation media."""

import json
import os
from pathlib import Path
import subprocess
import time

from prepare_target import validate_identity


def private_write(path, data):
    path.parent.mkdir(parents=True, exist_ok=True, mode=0o700)
    if path.is_symlink():
        raise ValueError(f"Refusing a credential symlink: {path}")
    fd = os.open(path, os.O_WRONLY | os.O_CREAT | os.O_TRUNC, 0o600)
    with os.fdopen(fd, "wb") as stream:
        os.fchmod(stream.fileno(), 0o600)
        stream.write(data if isinstance(data, bytes) else data.encode())


def public_key(path):
    return subprocess.check_output(["ssh-keygen", "-y", "-f", str(path)], text=True).strip()


def encrypt_for_host(data, recipient):
    return subprocess.check_output(
        ["age", "--armor", "--encrypt", "--recipient", recipient], input=data
    ).decode()


def decrypt_for_host(ciphertext, key):
    return subprocess.check_output(
        ["age", "--decrypt", "--identity", str(key)], input=ciphertext.encode()
    )


def validate_seed(seed, username=None, hostname=None, owner="other"):
    if seed.get("version") != 1:
        raise ValueError("Unsupported personal media format")
    validate_identity(seed["username"], seed["hostname"], "other")
    if (
        owner != "other"
        or (username and username != seed["username"])
        or (hostname and hostname != seed["hostname"])
    ):
        raise ValueError(
            f"This personal USB is for Another person / {seed['username']} / {seed['hostname']}"
        )
    keys = seed["authorizedKeys"]
    if not isinstance(keys, list) or any(
        not isinstance(key, str) or not key.startswith("ssh-ed25519 ") or "\n" in key
        for key in keys
    ):
        raise ValueError("Support access must use explicit Ed25519 public keys")
    if not isinstance(seed["accounts"], str) or not seed["accounts"].startswith("-----BEGIN AGE"):
        raise ValueError("Missing encrypted agent accounts")


def read_seed(path):
    if not path.exists():
        return None
    if path.stat().st_size > 1024 * 1024:
        raise ValueError("Personal media bundle exceeds 1 MiB")
    seed = json.loads(path.read_text())
    validate_seed(seed)
    return seed


def load_live_seed(bundle=Path("/iso/family-seed.age"), state=Path("/run/family-installer")):
    seed = read_seed(state / "seed.json")
    if bundle.exists() and (seed is None or not (state / "unlocked").exists()):
        raise ValueError("Unlock this personal USB with Start personal setup before installing")
    return seed


def install_identity(seed, key):
    if key.exists():
        if public_key(key) != seed["hostPublicKey"]:
            raise ValueError("Existing target host key differs from the prepared identity")
    else:
        private_write(key, seed["hostPrivateKey"])
        if public_key(key) != seed["hostPublicKey"]:
            key.unlink()
            raise ValueError("Prepared host key does not match its public key")
    key.chmod(0o600)
    key.with_suffix(".pub").write_text(seed["hostPublicKey"] + "\n")


def install_accounts(accounts, home, uid, gid):
    for name, relative in (("codex", ".codex/auth.json"), ("claude", ".claude/.credentials.json")):
        if name not in accounts:
            continue
        path = home / relative
        # Preserve refreshed credentials across boots and installation retries.
        if not path.exists():
            private_write(path, json.dumps(accounts[name]))
            os.chown(path, uid, gid)
            os.chown(path.parent, uid, gid)
    if "claude" in accounts and not (home / ".claude.json").exists():
        path = home / ".claude.json"
        private_write(path, json.dumps({"hasCompletedOnboarding": True}))
        os.chown(path, uid, gid)


def provision_target(seed, root, machine):
    validate_seed(seed)
    key = root / "etc/ssh/ssh_host_ed25519_key"
    install_identity(seed, key)
    state = root / "var/lib/family-installer"
    state.mkdir(parents=True, exist_ok=True, mode=0o700)
    if not (state / "accounts-handed-off").exists():
        private_write(state / "accounts.age", seed["accounts"])
    if seed.get("tailscaleTarget"):
        private_write(state / "tailscale.age", seed["tailscaleTarget"])
    (machine / "provisioning.json").write_text(
        json.dumps(
            {
                "username": seed["username"],
                "authorizedKeys": seed["authorizedKeys"],
                "tailscale": bool(seed.get("tailscaleTarget")),
            },
            indent=2,
        )
        + "\n"
    )
    (machine / "provisioning.nix").write_text(
        "{...}: let\n  seed = builtins.fromJSON (builtins.readFile ./provisioning.json);\n"
        "in {\n  imports = [../../modules/provisioned-system.nix];\n"
        "  familyInstaller.provisioning = {\n"
        "    enable = true;\n    inherit (seed) authorizedKeys tailscale;\n  };\n}\n"
    )


def refresh_target_accounts(root, seed, home=Path("/home/nixos")):
    root = root.resolve()
    if root == Path("/") or not (root / "srv/nixos-family-installer/.installer-source").is_file():
        raise ValueError("The target has not been prepared by this installer")
    key = root / "etc/ssh/ssh_host_ed25519_key"
    if public_key(key) != seed["hostPublicKey"]:
        raise ValueError("Target identity changed before account handoff")
    selected = json.loads(decrypt_for_host(seed["accounts"], key))
    state = root / "var/lib/family-installer"
    marker = state / "accounts-handed-off"
    accounts = {}
    paths = []
    for name, relative in (("codex", ".codex/auth.json"), ("claude", ".claude/.credentials.json")):
        if name in selected:
            path = home / relative
            paths.append(path)
            if marker.exists():
                continue
            if not path.is_file():
                raise ValueError(f"The live {name} account was removed; sign in before finishing")
            accounts[name] = json.loads(path.read_text())
    if not marker.exists():
        private_write(
            state / "accounts.age",
            encrypt_for_host(json.dumps(accounts).encode(), seed["hostPublicKey"]),
        )
        private_write(marker, "complete\n")
    for path in paths:
        path.unlink(missing_ok=True)


def stop_live_codex():
    command = ["--uid", "nixos", "--exact", "codex|.codex-wrapped"]
    result = subprocess.run(["pkill", "--signal", "TERM", *command], check=False)
    if result.returncode not in (0, 1):
        result.check_returncode()
    for _ in range(50):
        result = subprocess.run(["pgrep", *command], capture_output=True, check=False)
        if result.returncode == 1:
            return
        result.check_returncode()
        time.sleep(0.1)
    raise ValueError("Close the live Codex sessions before finishing account transfer")


def boot_target(
    username,
    state=Path("/var/lib/family-installer"),
    key=Path("/etc/ssh/ssh_host_ed25519_key"),
    runtime=Path("/run/family-installer"),
):
    import pwd

    user = pwd.getpwnam(username)
    encrypted = state / "accounts.age"
    if encrypted.exists():
        accounts = json.loads(decrypt_for_host(encrypted.read_text(), key))
        install_accounts(accounts, Path(user.pw_dir), user.pw_uid, user.pw_gid)
        encrypted.unlink()
    if (state / "tailscale.age").exists():
        private_write(
            runtime / "tailscale-authkey",
            decrypt_for_host((state / "tailscale.age").read_text(), key),
        )


def unlock_live(
    bundle, username, state=Path("/run/family-installer"), ready=Path("/run/family-installer.ready")
):
    import pwd

    result = subprocess.check_output(["age", "--decrypt", str(bundle)])
    seed = json.loads(result)
    validate_seed(seed)
    key = state / "target-host-key"
    install_identity(seed, key)
    accounts = json.loads(decrypt_for_host(seed["accounts"], key))
    user = pwd.getpwnam(username)
    install_accounts(accounts, Path(user.pw_dir), user.pw_uid, user.pw_gid)
    private_write(state / "seed.json", result)
    authorized = Path(user.pw_dir) / ".ssh/authorized_keys"
    if seed["authorizedKeys"]:
        existing = authorized.read_text() if authorized.exists() else ""
        missing = [key for key in seed["authorizedKeys"] if key not in existing.splitlines()]
        private_write(authorized, existing.rstrip() + "\n" + "\n".join(missing) + "\n")
        os.chown(authorized.parent, user.pw_uid, user.pw_gid)
        os.chown(authorized, user.pw_uid, user.pw_gid)
    if seed.get("tailscaleLive"):
        auth = state / "tailscale-live-authkey"
        private_write(auth, decrypt_for_host(seed["tailscaleLive"], key))
        try:
            subprocess.run(
                [
                    "tailscale",
                    "up",
                    f"--auth-key=file:{auth}",
                    "--hostname",
                    seed["hostname"] + "-installer",
                ],
                check=True,
            )
        finally:
            auth.unlink()
    private_write(state / "unlocked", "ready\n")
    ready.write_text(seed["hostname"] + "\n")
    print(f"Personal setup ready: Another person / {seed['username']} / {seed['hostname']}")


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(description=__doc__)
    sub = parser.add_subparsers(dest="command", required=True)
    boot = sub.add_parser("boot")
    boot.add_argument("username")
    unlock = sub.add_parser("unlock")
    unlock.add_argument("bundle", type=Path)
    unlock.add_argument("username")
    args = parser.parse_args()
    if args.command == "boot":
        boot_target(args.username)
    else:
        unlock_live(args.bundle, args.username)
