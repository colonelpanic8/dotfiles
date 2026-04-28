#!/usr/bin/env python3
import argparse
import json
import os
import sys
import threading
import time

import requests
from pykefcontrol.kef_connector import KefConnector
from zeroconf import IPVersion, ServiceBrowser, ServiceListener, Zeroconf


SOURCES = ("wifi", "bluetooth", "tv", "optic", "coaxial", "analog", "standby")
DISCOVERY_SERVICE_TYPES = (
    "_kef-info._tcp.local.",
    "_sues800device._tcp.local.",
    "_http._tcp.local.",
)


class KefDiscoveryListener(ServiceListener):
    def __init__(self):
        self._lock = threading.Lock()
        self.speakers = {}

    def add_service(self, zeroconf, service_type, name):
        self._record_service(zeroconf, service_type, name)

    def update_service(self, zeroconf, service_type, name):
        self._record_service(zeroconf, service_type, name)

    def remove_service(self, zeroconf, service_type, name):
        return None

    def _record_service(self, zeroconf, service_type, name):
        info = zeroconf.get_service_info(service_type, name, timeout=1000)
        if info is None:
            return

        properties = {
            key.decode("utf-8", errors="replace"): (
                value.decode("utf-8", errors="replace") if value is not None else ""
            )
            for key, value in info.properties.items()
        }
        addresses = [
            address
            for address in info.parsed_addresses(IPVersion.V4Only)
            if not address.startswith("127.")
        ]
        if not addresses:
            return

        is_kef = (
            service_type in ("_kef-info._tcp.local.", "_sues800device._tcp.local.")
            or properties.get("manufacturer", "").lower() == "kef"
            or "kef" in name.lower()
            or "ls50" in name.lower()
        )
        if not is_kef:
            return

        key = addresses[0]
        with self._lock:
            existing = self.speakers.get(key, {})
            self.speakers[key] = {
                **existing,
                "address": addresses[0],
                "hostname": (info.server.rstrip(".") if info.server else None)
                or existing.get("hostname"),
                "name": properties.get("name")
                or properties.get("fn")
                or existing.get("name")
                or strip_service_name(name),
                "model": properties.get("modelName")
                or properties.get("model")
                or properties.get("mn")
                or existing.get("model"),
                "version": properties.get("version") or existing.get("version"),
                "service": existing.get("service") or service_type.removesuffix(".local."),
            }


def strip_service_name(name):
    for suffix in DISCOVERY_SERVICE_TYPES:
        if name.endswith("." + suffix):
            return name[: -(len(suffix) + 1)]
    return name.rstrip(".")


def discover_speakers(timeout):
    listener = KefDiscoveryListener()
    zeroconf = Zeroconf(ip_version=IPVersion.V4Only)
    try:
        browsers = [
            ServiceBrowser(zeroconf, service_type, listener)
            for service_type in DISCOVERY_SERVICE_TYPES
        ]
        deadline = time.monotonic() + timeout
        while time.monotonic() < deadline:
            if listener.speakers:
                time.sleep(0.25)
                break
            time.sleep(0.05)
        return sorted(
            listener.speakers.values(),
            key=lambda speaker: (
                speaker.get("name") or "",
                speaker.get("address") or "",
            ),
        )
    finally:
        for browser in locals().get("browsers", []):
            browser.cancel()
        zeroconf.close()


def discover_host(timeout):
    speakers = discover_speakers(timeout)
    if not speakers:
        raise SystemExit(
            "Could not discover a KEF speaker. Pass --host <speaker-ip> or set KEF_HOST."
        )
    if len(speakers) > 1:
        choices = ", ".join(
            f"{speaker.get('name') or 'KEF'} at {speaker['address']}" for speaker in speakers
        )
        raise SystemExit(f"Multiple KEF speakers discovered: {choices}. Pass --host explicitly.")
    return speakers[0]["address"]


def connector(args):
    host = (
        args.host
        or os.environ.get("KEF_HOST")
        or os.environ.get("KEF_IP")
        or discover_host(args.discovery_timeout)
    )
    return KefConnector(host, model=args.model)


def print_status(speaker):
    info = {
        "name": speaker.speaker_name,
        "model": speaker.speaker_model,
        "firmware": speaker.firmware_version,
        "status": speaker.status,
        "source": speaker.source,
        "volume": speaker.volume,
        "playing": speaker.is_playing,
    }
    try:
        song = speaker.get_song_information()
        if any(song.values()):
            info["song"] = song
    except (KeyError, IndexError, TypeError, requests.RequestException):
        pass
    print(json.dumps(info, indent=2, sort_keys=True))


def bounded_volume(value):
    return max(0, min(100, value))


def main():
    parser = argparse.ArgumentParser(prog="kef", description="Control KEF W2 speakers.")
    parser.add_argument(
        "--host",
        help="Speaker IP address. Defaults to KEF_HOST/KEF_IP, then mDNS discovery.",
    )
    parser.add_argument(
        "--model",
        default="LS50W2",
        help="Speaker model passed to pykefcontrol. Default: LS50W2.",
    )
    parser.add_argument(
        "--discovery-timeout",
        default=2.0,
        type=float,
        help="Seconds to wait for mDNS discovery when --host/KEF_HOST is unset.",
    )
    subparsers = parser.add_subparsers(dest="command", required=True)

    subparsers.add_parser("discover", help="List discovered KEF speakers as JSON.")
    subparsers.add_parser("status", help="Print speaker status as JSON.")
    subparsers.add_parser("on", help="Power on.")
    subparsers.add_parser("standby", help="Put the speaker in standby.")
    subparsers.add_parser("mute", help="Mute by setting volume to 0.")

    volume = subparsers.add_parser("volume", help="Get or set volume.")
    volume.add_argument("level", nargs="?", type=int, help="Volume level, 0-100.")

    up = subparsers.add_parser("up", help="Increase volume.")
    up.add_argument("step", nargs="?", default=3, type=int)

    down = subparsers.add_parser("down", help="Decrease volume.")
    down.add_argument("step", nargs="?", default=3, type=int)

    source = subparsers.add_parser("source", help="Get or set source.")
    source.add_argument("name", nargs="?", choices=SOURCES)

    subparsers.add_parser("play-pause", help="Toggle play/pause.")
    subparsers.add_parser("next", help="Next track.")
    subparsers.add_parser("previous", help="Previous track.")

    args = parser.parse_args()
    if args.command == "discover":
        print(json.dumps(discover_speakers(args.discovery_timeout), indent=2, sort_keys=True))
        return 0

    speaker = connector(args)

    try:
        if args.command == "status":
            print_status(speaker)
        elif args.command == "on":
            speaker.power_on()
        elif args.command == "standby":
            speaker.shutdown()
        elif args.command == "mute":
            speaker.volume = 0
        elif args.command == "volume":
            if args.level is None:
                print(speaker.volume)
            else:
                speaker.volume = bounded_volume(args.level)
        elif args.command == "up":
            speaker.volume = bounded_volume(speaker.volume + args.step)
        elif args.command == "down":
            speaker.volume = bounded_volume(speaker.volume - args.step)
        elif args.command == "source":
            if args.name is None:
                print(speaker.source)
            else:
                speaker.source = args.name
        elif args.command == "play-pause":
            speaker.toggle_play_pause()
        elif args.command == "next":
            speaker.next_track()
        elif args.command == "previous":
            speaker.previous_track()
    except requests.RequestException as error:
        print(f"kef: failed to reach speaker: {error}", file=sys.stderr)
        return 1

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
