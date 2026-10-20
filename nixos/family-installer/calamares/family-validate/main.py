"""Validate account and machine identity before partitioning."""

import subprocess

import libcalamares


def pretty_name():
    return "Checking account and computer name"


def run():
    storage = libcalamares.globalstorage
    result = subprocess.run(
        [
            "@helper@",
            "--validate",
            "--username",
            storage.value("username") or "",
            "--hostname",
            storage.value("hostname") or "",
            "--owner",
            storage.value("packagechooser_owner") or "",
        ],
        capture_output=True,
        text=True,
        check=False,
    )
    if result.returncode:
        return (
            "Check the account and computer name",
            result.stderr.rsplit("error:", 1)[-1].strip(),
        )
    return None
