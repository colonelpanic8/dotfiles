#!/usr/bin/python3
"""Let macOS find Determinate's existing Nix Store password by volume UUID."""

import os
import plistlib
import subprocess
import sys
import uuid


def run(*args, input=None):
    result = subprocess.run(args, input=input, capture_output=True)
    if result.returncode:
        # Never include command input/output: it may contain the volume password.
        raise RuntimeError(f"{args[0]} {args[1]} failed (exit {result.returncode})")
    return result.stdout


def main():
    if os.geteuid() != 0:
        raise RuntimeError("Run with sudo /usr/bin/python3 " + __file__)

    info = plistlib.loads(run("/usr/sbin/diskutil", "info", "-plist", "/nix"))
    if info.get("MountPoint") != "/nix" or info.get("VolumeName") != "Nix Store":
        raise RuntimeError("Expected the Nix Store APFS volume mounted at /nix")
    if not info.get("FileVault"):
        raise RuntimeError("The Nix Store volume is not encrypted")
    volume_uuid = str(uuid.UUID(info["VolumeUUID"])).upper()
    keychain = "/Library/Keychains/System.keychain"
    password = run(
        "/usr/bin/security", "find-generic-password",
        "-a", "Nix Store", "-s", "Nix Store", "-w", keychain,
    ).removesuffix(b"\n")
    if not password:
        raise RuntimeError("The installer Keychain password is empty")

    # -verify checks the credential without unmounting or locking a live store.
    run(
        "/usr/sbin/diskutil", "apfs", "unlockVolume", info["DeviceIdentifier"],
        "-user", volume_uuid, "-verify", "-stdinpassphrase", input=password + b"\n",
    )

    lookup = (
        "/usr/bin/security", "find-generic-password",
        "-a", volume_uuid, "-s", volume_uuid, "-w", keychain,
    )
    existing = subprocess.run(lookup, capture_output=True)
    if existing.returncode == 0:
        if existing.stdout.removesuffix(b"\n") != password:
            raise RuntimeError("An existing UUID Keychain entry has a different password")
        print(f"Verified existing macOS UUID lookup for Nix Store: {volume_uuid}")
        return
    if existing.returncode != 44:  # errSecItemNotFound
        raise RuntimeError("Could not check the existing UUID Keychain entry")

    # Feed security's interactive mode over stdin, keeping secrets out of argv
    # and files. Hex encoding avoids its command parser interpreting passwords.
    command = (
        f'add-generic-password -a {volume_uuid} -s {volume_uuid} '
        '-l "Nix Store" -D "Encrypted Volume Password" '
        '-j "UUID lookup for macOS automatic Nix Store unlock" '
        f'-X {password.hex()} '
        '-T /System/Library/CoreServices/APFSUserAgent '
        '-T /System/Library/CoreServices/CSUserAgent '
        f'-T /usr/bin/security {keychain}\n'
    )
    run("/usr/bin/security", "-i", input=command.encode())
    saved = run(*lookup).removesuffix(b"\n")
    if saved != password:
        raise RuntimeError("UUID Keychain entry did not match the verified password")
    print(f"Verified Nix Store password and saved macOS UUID lookup: {volume_uuid}")


if __name__ == "__main__":
    try:
        main()
    except (RuntimeError, KeyError, ValueError) as error:
        sys.exit(str(error))
