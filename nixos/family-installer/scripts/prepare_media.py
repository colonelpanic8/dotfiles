"""Build personal USB media from this checkout, without publishing credentials."""

import argparse
import base64
import json
import os
from pathlib import Path
import platform
import plistlib
import shutil
import subprocess
import sys

from prepare_target import validate_identity
from provisioning import encrypt_for_host, public_key


def run(*args, **kwargs):
    return subprocess.run(list(map(str, args)), check=True, **kwargs)


def codex_credentials(home, email):
    path = home / "auth.json"
    if not path.is_file():
        raise ValueError("Sign in to the dedicated installer Codex profile first")
    auth = json.loads(path.read_text())
    tokens = auth.get("tokens") or {}
    if not tokens.get("refresh_token") or not tokens.get("access_token"):
        raise ValueError("A refreshable ChatGPT/Codex login is required")
    try:
        identity = json.loads(base64.urlsafe_b64decode(tokens["id_token"].split(".")[1] + "==="))
    except (KeyError, ValueError, IndexError):
        raise ValueError("Cannot identify the Codex account") from None
    if identity.get("email", "").casefold() != email.casefold():
        raise ValueError("The signed-in Codex account does not match --email")
    run(
        "codex",
        "-c",
        'cli_auth_credentials_store="file"',
        "login",
        "status",
        env={**os.environ, "CODEX_HOME": str(home)},
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
    )
    return auth


def prepare_identity(args, state):
    validate_identity(args.username, args.hostname, "other")
    login = state / "codex-login"
    login.mkdir(exist_ok=True, mode=0o700)
    if not (login / "auth.json").exists():
        print(
            "Sign in to Codex for the new PC. Your Mac's existing login stays separate.", flush=True
        )
        run(
            "codex",
            "-c",
            'cli_auth_credentials_store="file"',
            "login",
            env={**os.environ, "CODEX_HOME": str(login)},
        )
    accounts = {"codex": codex_credentials(login, args.email)}
    key = state / "ssh_host_ed25519_key"
    if not key.exists():
        run("ssh-keygen", "-q", "-t", "ed25519", "-N", "", "-C", args.hostname, "-f", key)
    key.chmod(0o600)
    recipient = public_key(key)
    authorized = []
    for path in args.authorized_key:
        authorized.extend(line.strip() for line in path.read_text().splitlines() if line.strip())
    seed = {
        "version": 1,
        "username": args.username,
        "hostname": args.hostname,
        "hostPrivateKey": key.read_text(),
        "hostPublicKey": recipient,
        "authorizedKeys": authorized,
        "accounts": encrypt_for_host(json.dumps(accounts).encode(), recipient),
    }
    for field, path in (
        ("tailscaleLive", args.tailscale_live_key),
        ("tailscaleTarget", args.tailscale_target_key),
    ):
        if path:
            seed[field] = encrypt_for_host(path.read_bytes(), recipient)
    from provisioning import validate_seed

    validate_seed(seed)
    (state / "identity.json").write_text(
        json.dumps(
            {k: seed[k] for k in ("username", "hostname", "hostPublicKey", "authorizedKeys")},
            indent=2,
        )
        + "\n"
    )
    return seed


def seal_seed(seed, state):
    bundle = state / "family-seed.age"
    temporary = state / "family-seed.age.new"
    temporary.unlink(missing_ok=True)
    print(
        "Choose a USB bundle passphrase. Enter it again after booting the new computer.", flush=True
    )
    try:
        run("age", "--passphrase", "--output", temporary, input=json.dumps(seed).encode())
        temporary.chmod(0o600)
        temporary.replace(bundle)
    finally:
        temporary.unlink(missing_ok=True)
    return bundle


def build_iso(source, state):
    link = state / "base-image"
    run("nix", "build", str(source) + "#packages.x86_64-linux.iso", "--out-link", link)
    images = list((link / "iso").glob("*.iso"))
    if len(images) != 1:
        raise ValueError("Expected exactly one ISO from the build")
    return images[0]


def personalize_iso(base, bundle, output):
    temporary = output.with_suffix(".iso.new")
    temporary.unlink(missing_ok=True)
    try:
        run(
            "xorriso",
            "-indev",
            base,
            "-outdev",
            temporary,
            "-boot_image",
            "any",
            "replay",
            "-map",
            bundle,
            "/family-seed.age",
            "-commit",
            "-end",
        )
        temporary.chmod(0o600)
        temporary.replace(output)
    finally:
        temporary.unlink(missing_ok=True)


def inspect_usb(device, image_size):
    if platform.system() != "Darwin":
        raise ValueError("Automatic writing currently supports macOS only")
    data = subprocess.check_output(["diskutil", "info", "-plist", device])
    info = plistlib.loads(data)
    if (
        not info.get("WholeDisk")
        or info.get("Internal", True)
        or info.get("BusProtocol") != "USB"
        or not info.get("Writable", False)
        or info.get("TotalSize", 0) < image_size
    ):
        raise ValueError("Select a writable external whole USB disk with enough space")
    return info


def write_usb(image, device):
    info = inspect_usb(device, image.stat().st_size)
    identifier = info["DeviceIdentifier"]
    print(f"ERASE ALL DATA: {identifier}, {info.get('MediaName')}, {info['TotalSize']} bytes")
    if input(f"Type ERASE {identifier} to write this USB: ") != f"ERASE {identifier}":
        raise ValueError("USB write cancelled")
    current = inspect_usb(device, image.stat().st_size)
    for key in ("DeviceIdentifier", "TotalSize", "MediaName", "DeviceTreePath"):
        if current.get(key) != info.get(key):
            raise ValueError("USB identity changed; inspect devices again")
    run("diskutil", "unmountDisk", device)
    run("sudo", "dd", f"if={image}", f"of=/dev/r{identifier}", "bs=4194304")
    run("sync")
    # Compare only the written image length; the USB is usually larger.
    run("sudo", "cmp", "-n", str(image.stat().st_size), image, f"/dev/r{identifier}")
    run("diskutil", "eject", device)
    print("USB write and byte-for-byte verification complete.")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--username", default="alex")
    parser.add_argument("--hostname", default="alex-nixos")
    parser.add_argument("--email", default="alexmalison@gmail.com")
    parser.add_argument("--source", type=Path, default=Path(__file__).resolve().parents[1])
    parser.add_argument("--iso", type=Path, help="Use a previously built and verified base ISO")
    parser.add_argument(
        "--stage-only",
        action="store_true",
        help="Prepare identity and encrypted accounts without building or writing media",
    )
    parser.add_argument("--authorized-key", type=Path, action="append", default=[])
    parser.add_argument("--tailscale-live-key", type=Path)
    parser.add_argument("--tailscale-target-key", type=Path)
    parser.add_argument("--device", help="Confirmed external whole USB disk, e.g. /dev/disk4")
    parser.add_argument(
        "--write-existing", type=Path, help="Write an already prepared ISO without rebuilding"
    )
    args = parser.parse_args()
    os.umask(0o077)
    for executable in ("age", "xorriso", "ssh-keygen", "codex"):
        if not shutil.which(executable):
            raise ValueError(
                f"Missing {executable}; run through `just family-usb` in the Nix shell"
            )
    if args.write_existing:
        if not args.device:
            parser.error("--write-existing requires --device")
        write_usb(args.write_existing.resolve(), args.device)
        return
    validate_identity(args.username, args.hostname, "other")
    state = Path.home() / ".local/state/nixos-family-installer" / args.hostname
    state.mkdir(parents=True, exist_ok=True, mode=0o700)
    # These local files must never become flake inputs or public release assets.
    if state.is_relative_to(args.source.resolve()):
        raise ValueError("Private preparation state must be outside the source checkout")
    if args.iso and not args.iso.is_file():
        raise ValueError("The base ISO does not exist")
    seed = prepare_identity(args, state)
    if args.stage_only:
        print(f"Prepared public identity: {state / 'identity.json'}")
        return
    base = args.iso.resolve() if args.iso else build_iso(args.source.resolve(), state)
    if not base.is_file():
        raise ValueError("The base ISO does not exist")
    bundle = seal_seed(seed, state)
    output = state / f"{args.hostname}-installer.iso"
    personalize_iso(base, bundle, output)
    (state / "codex-login/auth.json").unlink()
    print(f"Private installer: {output}\nPublic identity: {state / 'identity.json'}")
    if args.device:
        write_usb(output, args.device)
    else:
        print(
            "Plug in the USB, inspect `diskutil list external physical`, then use --write-existing and --device."
        )


if __name__ == "__main__":
    try:
        main()
    except (ValueError, subprocess.CalledProcessError) as error:
        print(f"Preparation stopped: {error}", file=sys.stderr)
        sys.exit(1)
