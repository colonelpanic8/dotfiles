"""Manage the dedicated OBS camera/microphone profile without changing other profiles."""

import argparse
import configparser
import json
import os
import pwd
import shlex
import signal
import socket
import subprocess
import time
from pathlib import Path

NAME = "TailnetCamera"
OBS = "/Applications/OBS.app"


def source(name, kind, settings):
    return {
        "name": name,
        "id": kind,
        "versioned_id": kind,
        "settings": settings,
        "mixers": 1,
        "volume": 1.0,
        "enabled": True,
        "muted": False,
    }


def collection(camera_id, microphone):
    sources = []
    if camera_id:
        sources.append(
            source(
                "Camera",
                "macos-avcapture",
                {
                    "device": camera_id,
                    "use_preset": True,
                    "preset": "AVCaptureSessionPreset1280x720",
                    "enable_audio": False,
                },
            )
        )
    if microphone:
        sources.append(
            source("Microphone", "coreaudio_input_capture", {"device_id": "default"})
        )
    items = [
        {
            "name": s["name"],
            "visible": True,
            "id": i + 1,
            "pos": {"x": 0.0, "y": 0.0},
            "scale": {"x": 1.0, "y": 1.0},
            "bounds_type": 2,
            "bounds": {"x": 1280.0, "y": 720.0},
        }
        for i, s in enumerate(sources)
    ]
    sources.append(source(NAME, "scene", {"items": items}))
    return {
        "name": NAME,
        "current_scene": NAME,
        "current_program_scene": NAME,
        "scene_order": [{"name": NAME}],
        "sources": sources,
        "groups": [],
        "transitions": [],
        "current_transition": "Fade",
    }


def update_ini(path, sections):
    config = configparser.ConfigParser(interpolation=None)
    config.optionxform = str
    config.read(path)
    for name, values in sections.items():
        if name not in config:
            config[name] = {}
        config[name].update(values)
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w") as output:
        config.write(output, space_around_delimiters=False)


def prepare(home, inventory):
    root = home / "Library/Application Support/obs-studio"
    profile = root / "basic/profiles" / NAME
    camera = next(iter(inventory.get("SPCameraDataType", [])), {})
    audio = [
        device
        for group in inventory.get("SPAudioDataType", [])
        for device in group.get("_items", [])
    ]
    microphone = any(
        int(device.get("coreaudio_device_input", 0)) > 0 for device in audio
    )
    camera_id = camera.get("spcamera_unique-id")
    if not camera_id and not microphone:
        print("No camera or microphone connected; capture remains stopped.", flush=True)
        return False

    update_ini(
        profile / "basic.ini",
        {
            "General": {"Name": NAME},
            "Output": {
                "Mode": "Simple",
                "Reconnect": "true",
                "RetryDelay": "5",
                "MaxRetries": "10000",
            },
            "SimpleOutput": {
                "StreamEncoder": "apple_h264",
                "VBitrate": "1800",
                "ABitrate": "96",
                "UseAdvanced": "true",
                "Preset": "veryfast",
            },
            "Video": {
                "BaseCX": "1280",
                "BaseCY": "720",
                "OutputCX": "1280",
                "OutputCY": "720",
                "FPSType": "1",
                "FPSInt": "15",
            },
            "Audio": {"SampleRate": "48000", "ChannelSetup": "Mono"},
        },
    )
    (profile / "service.json").write_text(
        json.dumps(
            {
                "type": "rtmp_custom",
                "settings": {
                    "server": "rtmp://127.0.0.1:18379",
                    "key": "camera",
                    "use_auth": False,
                },
            }
        )
    )
    scenes = root / "basic/scenes"
    scenes.mkdir(parents=True, exist_ok=True)
    (scenes / f"{NAME}.json").write_text(json.dumps(collection(camera_id, microphone)))
    # OBS 31+ splits user.ini from global.ini; set both for older installations.
    for name in ("global.ini", "user.ini"):
        update_ini(
            root / name,
            {
                "General": {"FirstRun": "true"},
                "Basic": {"ConfigOnNewProfile": "false"},
                "BasicWindow": {
                    "SysTrayEnabled": "true",
                    "WarnBeforeStartingStream": "false",
                    "WarnBeforeStoppingStream": "false",
                    "WarnBeforeStoppingRecord": "false",
                },
            },
        )
    return True


def obs_processes():
    output = subprocess.check_output(
        ["/bin/ps", "-ww", "-U", str(os.getuid()), "-o", "pid=,command="], text=True
    )
    return [
        (int(pid), command)
        for line in output.splitlines()
        for pid, command in [line.strip().split(None, 1)]
        if command.startswith(OBS + "/Contents/MacOS/OBS")
    ]


def managed(command):
    arguments = shlex.split(command)
    return any(
        pair == ["--collection", NAME]
        for pair in (arguments[i : i + 2] for i in range(len(arguments) - 1))
    )


def publishing(processes):
    pids = [str(pid) for pid, command in processes if managed(command)]
    if not pids:
        return False
    result = subprocess.run(
        [
            "/usr/sbin/lsof",
            "-a",
            "-p",
            ",".join(pids),
            "-iTCP@127.0.0.1:18379",
            "-sTCP:ESTABLISHED",
            "-t",
        ],
        capture_output=True,
        text=True,
        check=False,
    )
    return result.returncode == 0


def stop_capture(paused, processes):
    paused.touch()
    for pid, command in processes:
        if managed(command):
            # OBS prompts on SIGTERM while streaming; Stop must release capture immediately.
            try:
                os.kill(pid, signal.SIGKILL)
            except ProcessLookupError:
                pass


def start_capture(paused, processes):
    if any(not managed(command) for _, command in processes):
        raise RuntimeError(
            "OBS is using another collection; leaving that session alone."
        )
    if publishing(processes):
        paused.unlink(missing_ok=True)
        print("Camera and microphone are already publishing.")
        return
    stop_capture(paused, processes)
    paused.unlink(missing_ok=True)
    subprocess.run(
        [
            "/bin/launchctl",
            "kickstart",
            "-k",
            f"user/{os.getuid()}/org.nix-community.home.mac-camera",
        ],
        check=True,
    )
    print("Camera and microphone start requested.")


def wait_for_relay(paused):
    while not paused.exists():
        try:
            with socket.create_connection(("127.0.0.1", 18379), timeout=2):
                return True
        except OSError:
            time.sleep(2)
    return False


def clear_run_markers(home):
    # OBS 32's empty run markers otherwise make unattended restarts wait on a crash dialog.
    directory = home / "Library/Application Support/obs-studio/.sentinel"
    for marker in directory.glob("run_*"):
        if marker.is_file() and marker.stat().st_size == 0:
            marker.unlink(missing_ok=True)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--owner", required=True)
    parser.add_argument(
        "action", choices=["run", "start", "stop", "pause", "resume", "status"]
    )
    parser.add_argument("--json", action="store_true", help="Output status as JSON")
    args = parser.parse_args()
    if pwd.getpwuid(os.getuid()).pw_name != args.owner:
        parser.error(f"Run mac-camera as this Mac's primary user: {args.owner}")
    home = Path.home()
    state = home / "Library/Application Support/TailnetCamera"
    state.mkdir(parents=True, exist_ok=True)
    paused = state / "paused"
    if args.action in ("pause", "stop"):
        stop_capture(paused, obs_processes())
        print("Camera and microphone paused; this survives login and rebuilds.")
    elif args.action in ("resume", "start"):
        try:
            start_capture(paused, obs_processes())
        except RuntimeError as error:
            parser.exit(1, f"{error}\n")
    elif args.action == "status":
        processes = obs_processes()
        status = {
            "paused": paused.exists(),
            "obsRunning": any(managed(command) for _, command in processes),
            "publishing": publishing(processes),
        }
        if args.json:
            print(json.dumps(status))
        else:
            print("Paused" if status["paused"] else "Enabled at login")
            print("Publishing" if status["publishing"] else "Not publishing")
    else:
        if paused.exists():
            return
        while subprocess.run(
            ["/bin/launchctl", "print", f"gui/{os.getuid()}"],
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
            check=False,
        ).returncode:
            time.sleep(5)
            if paused.exists():
                return
        if obs_processes():
            print("OBS is already running; leaving its session alone.")
            return
        clear_run_markers(home)
        inventory = json.loads(
            subprocess.check_output(
                [
                    "/usr/sbin/system_profiler",
                    "SPCameraDataType",
                    "SPAudioDataType",
                    "-json",
                ]
            )
        )
        if prepare(home, inventory) and wait_for_relay(paused):
            subprocess.run(
                [
                    "/usr/bin/open",
                    "-W",
                    "-g",
                    "-j",
                    "-a",
                    OBS,
                    "--args",
                    "--profile",
                    NAME,
                    "--collection",
                    NAME,
                    "--startstreaming",
                    "--minimize-to-tray",
                    "--disable-updater",
                ],
                check=True,
            )


if __name__ == "__main__":
    main()
