"""Prepare a flake from Calamares' generated configuration on the target disk."""

import argparse
import json
import os
from pathlib import Path
import re
import shutil
import subprocess


def validate_identity(username, hostname, owner):
    if (
        len(username) > 31
        or not re.fullmatch(r"[a-z_][a-z0-9_-]*\$?", username)
        or username in {"root", "nobody"}
    ):
        raise ValueError("Choose a regular lowercase login name, not root")
    if hostname.lower() in {"localhost", "installer"} or not re.fullmatch(
        r"[A-Za-z0-9](?:[A-Za-z0-9_-]{0,61}[A-Za-z0-9])?", hostname
    ):
        raise ValueError("Use a computer name starting and ending with a letter or digit")
    if owner not in {"ivan", "other"}:
        raise ValueError("Choose Ivan or Another person")


def prepare_target(source, root, username, hostname, owner, seed=None):
    validate_identity(username, hostname, owner)
    if seed:
        from provisioning import validate_seed

        validate_seed(seed, username, hostname, owner)
    root = root.resolve()
    source = source.resolve()
    if root == Path("/") or root == source or root in source.parents or source in root.parents:
        raise ValueError("The destination must be a separate mounted installation target")
    generated = root / "etc/nixos"
    for name in ("configuration.nix", "hardware-configuration.nix"):
        if not (generated / name).is_file():
            raise ValueError(f"Calamares has not generated {name}")
    if not (source / "flake.lock").is_file():
        raise ValueError("The installer source must have a pinned flake.lock")

    destination = root / "srv/nixos-family-installer"
    marker = destination / ".installer-source"
    if destination.exists() and (not marker.is_file() or marker.read_text().strip() != str(source)):
        raise ValueError("Refusing to overwrite an existing configuration checkout")
    destination.mkdir(parents=True, exist_ok=True)
    marker.write_text(str(source) + "\n")
    shutil.copytree(
        source,
        destination,
        dirs_exist_ok=True,
        ignore=shutil.ignore_patterns(
            ".git", ".direnv", ".installer-source", "__pycache__", "result", "result-*"
        ),
    )
    for directory, _, files in os.walk(destination):
        path = Path(directory)
        path.chmod(path.stat().st_mode | 0o200)
        for name in files:
            path = Path(directory) / name
            path.chmod(path.stat().st_mode | 0o200)
    machine = destination / "machines" / hostname
    machine.mkdir(parents=True, exist_ok=True)
    for name in ("configuration.nix", "hardware-configuration.nix"):
        shutil.copyfile(generated / name, machine / name)
    identity = {"primaryUser": username, "owner": owner, "hostname": hostname}
    (machine / "identity.json").write_text(json.dumps(identity, indent=2) + "\n")
    (machine / "default.nix").write_text(
        "{...}: let\n"
        "  identity = builtins.fromJSON (builtins.readFile ./identity.json);\n"
        "in {\n"
        f"  imports = [./configuration.nix{' ./provisioning.nix' if seed else ''}];\n"
        "  myModules.primaryUser = identity.primaryUser;\n"
        "  myModules.userProfile.owner = identity.owner;\n"
        "}\n"
    )

    host_key = root / "etc/ssh/ssh_host_ed25519_key"
    host_key.parent.mkdir(parents=True, exist_ok=True)
    if seed:
        from provisioning import provision_target

        provision_target(seed, root, machine)
    if not host_key.exists():
        subprocess.run(
            ["ssh-keygen", "-q", "-t", "ed25519", "-N", "", "-C", hostname, "-f", str(host_key)],
            check=True,
            stdout=subprocess.DEVNULL,
        )
    public_key = subprocess.check_output(
        ["ssh-keygen", "-y", "-f", str(host_key)], text=True
    ).strip()
    host_key.with_suffix(".pub").write_text(public_key + "\n")
    request = root / "var/lib/nixos-family-installer/enrollment.json"
    request.parent.mkdir(parents=True, exist_ok=True)
    request.write_text(
        json.dumps(
            {
                **identity,
                "hostPublicKey": public_key,
                "requestedSecrets": [],
                "managedUpdates": "pending-registration",
                "machineDirectory": f"machines/{hostname}",
                "installerSource": str(source),
            },
            indent=2,
        )
        + "\n"
    )
    return f"{destination}#{hostname}"


def finish_target(root):
    root = root.resolve()
    destination = root / "srv/nixos-family-installer"
    configuration = root / "etc/nixos"
    if root == Path("/") or not (destination / ".installer-source").is_file():
        raise ValueError("The target has not been prepared by this installer")
    if configuration.is_symlink() and configuration.resolve() == destination:
        return
    backup = root / "etc/nixos.calamares"
    if backup.exists():
        raise ValueError("Refusing to overwrite the original Calamares configuration backup")
    configuration.rename(backup)
    configuration.symlink_to("../srv/nixos-family-installer", target_is_directory=True)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--source", required=True)
    for name in ("root", "username", "hostname", "owner"):
        parser.add_argument(f"--{name}")
    parser.add_argument("--finish", action="store_true")
    parser.add_argument("--validate", action="store_true")
    args = parser.parse_args()
    from provisioning import load_live_seed, refresh_target_accounts, stop_live_codex, validate_seed

    try:
        seed = load_live_seed()
    except ValueError as error:
        parser.error(str(error))
    if args.finish:
        if not args.root:
            parser.error("--root is required to finish installation")
        finish_target(Path(args.root))
        if seed:
            stop_live_codex()
            refresh_target_accounts(Path(args.root), seed)
        return
    if not all((args.username, args.hostname, args.owner)):
        parser.error("--username, --hostname and --owner are required for preparation")
    try:
        validate_identity(args.username, args.hostname, args.owner)
        if seed:
            validate_seed(seed, args.username, args.hostname, args.owner)
    except ValueError as error:
        parser.error(str(error))
    if args.validate:
        return
    if not args.root:
        parser.error("--root is required for preparation")
    print(
        prepare_target(
            Path(args.source), Path(args.root), args.username, args.hostname, args.owner, seed
        )
    )


if __name__ == "__main__":
    main()
