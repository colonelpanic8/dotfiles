"""Publish the existing tray policy and optional RAPL samples to Quickshell."""

import argparse
import fcntl
import json
import os
import re
import tempfile
import time
from pathlib import Path

import dbus
import yaml


def priority_path():
    return (
        Path(os.environ.get("XDG_CONFIG_HOME", Path.home() / ".config"))
        / "taffybar/sni-priorities.yaml"
    )


def read_policy(path):
    try:
        policy = yaml.safe_load(path.read_text()) or {}
        priorities = policy.get("priorities", {})
        if isinstance(priorities, list):
            priorities = {entry["key"]: entry["priority"] for entry in priorities}
        return policy, priorities
    except (OSError, ValueError, TypeError, KeyError, yaml.YAMLError):
        return {}, {}


def update_policy(path, change):
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.with_suffix(".lock").open("w") as lock:
        fcntl.flock(lock, fcntl.LOCK_EX)
        policy, priorities = read_policy(path)
        change(policy, priorities)
        # Match taffybar's format_version 2 layout.
        policy["priorities"] = [
            {"key": key, "priority": priority}
            for key, priority in sorted(
                priorities.items(), key=lambda item: (-item[1], item[0])
            )
        ]
        with tempfile.NamedTemporaryFile(
            mode="w", dir=path.parent, delete=False
        ) as out:
            yaml.safe_dump(policy, out, sort_keys=False)
        os.replace(out.name, path)


def adjust_priority(path, key, delta):
    def change(policy, priorities):
        priorities[key] = max(-10, min(10, priorities.get(key, 0) + delta))

    update_policy(path, change)


def process_key(pid):
    try:
        args = Path(f"/proc/{pid}/cmdline").read_bytes().decode().split("\0")
        names = [Path(arg).name.lower() for arg in args if arg]
        for known in ("slack", "tailscale", "git-sync-rs", "gocryptfs-tray"):
            if any(known in name for name in names):
                return "process:" + known
        return "process:" + re.sub(r"^\.|-wrapped$", "", names[0])
    except (OSError, UnicodeError, IndexError):
        return ""


def tray_metadata(bus, known):
    watcher = bus.get_object(
        "org.kde.StatusNotifierWatcher", "/StatusNotifierWatcher", introspect=False
    )
    registrations = watcher.Get(
        "org.kde.StatusNotifierWatcher",
        "RegisteredStatusNotifierItems",
        dbus_interface="org.freedesktop.DBus.Properties",
        timeout=2,
    )
    entries = {}
    for registration in registrations:
        if str(registration) in known:
            entries[str(registration)] = known[str(registration)]
            continue
        service, _, suffix = str(registration).partition("/")
        path = "/" + suffix if suffix else "/StatusNotifierItem"
        try:
            item = bus.get_object(service, path, introspect=False)
            props = item.GetAll(
                "org.kde.StatusNotifierItem",
                dbus_interface="org.freedesktop.DBus.Properties",
                timeout=1,
            )
            if not props:
                props = item.GetAll(
                    "org.freedesktop.StatusNotifierItem",
                    dbus_interface="org.freedesktop.DBus.Properties",
                    timeout=1,
                )
            item_id = str(props.get("Id", ""))
            keys = [
                process_key(
                    bus.call_blocking(
                        "org.freedesktop.DBus",
                        "/org/freedesktop/DBus",
                        "org.freedesktop.DBus",
                        "GetConnectionUnixProcessID",
                        "s",
                        (service,),
                        timeout=1,
                    )
                ),
                "icon-name:" + str(props.get("IconName", "")),
                "item-id:" + item_id,
                "icon-title:" + str(props.get("ToolTip", ["", [], ""])[2]),
            ]
            entries[str(registration)] = keys
        except (dbus.DBusException, IndexError):
            continue
    return entries


class PowerSampler:
    def __init__(self):
        self.previous = {}

    def sample(self):
        total = 0.0
        found = False
        now = time.monotonic()
        for path in Path("/sys/class/powercap").glob("intel-rapl:*"):
            try:
                if not (path / "name").read_text().strip().startswith("package"):
                    continue
                energy = int((path / "energy_uj").read_text())
                maximum = int((path / "max_energy_range_uj").read_text())
                previous = self.previous.get(path)
                self.previous[path] = (now, energy)
                if previous:
                    total += (
                        ((energy - previous[1]) % maximum) / (now - previous[0]) / 1e6
                    )
                    found = True
            except (OSError, ValueError):
                continue
        return round(total, 1) if found else None


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--watch", action="store_true")
    parser.add_argument("--priority", nargs=2, metavar=("KEY", "DELTA"))
    args = parser.parse_args()
    if args.priority:
        adjust_priority(priority_path(), args.priority[0], int(args.priority[1]))
        return

    bus = dbus.SessionBus()
    power = PowerSampler()
    metadata = {}
    refreshed = 0
    published = None
    sampled = 0
    while True:
        _, priorities = read_policy(priority_path())
        # Pick up new registrations every tick; re-read existing items' properties less often.
        if time.monotonic() - refreshed >= 15:
            metadata = {}
            refreshed = time.monotonic()
        try:
            metadata = tray_metadata(bus, metadata)
        except dbus.DBusException:
            metadata = {}
        items = {}
        for identity, keys in metadata.items():
            key = next((key for key in keys if key in priorities), keys[0] or keys[2])
            items[identity] = {"key": key, "priority": priorities.get(key, 0)}
        tray = {"tray": items}
        if tray != published or time.monotonic() - sampled >= 5:
            print(json.dumps({**tray, "cpuWatts": power.sample()}), flush=True)
            published = tray
            sampled = time.monotonic()
        if not args.watch:
            return
        time.sleep(1)


if __name__ == "__main__":
    main()
