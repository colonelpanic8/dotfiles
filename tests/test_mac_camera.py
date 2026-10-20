import http.client
import importlib.util
import json
import tempfile
import threading
import unittest
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path
from unittest.mock import patch


def load(name):
    path = Path(__file__).resolve().parents[1] / "nix-darwin/camera" / f"{name}.py"
    spec = importlib.util.spec_from_file_location(name, path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


proxy = load("proxy")
capture = load("capture")


class Upstream(BaseHTTPRequestHandler):
    def do_GET(self):
        self.server.requests.append(self.path)
        data = b"test-media-bytes"
        self.send_response(200)
        self.send_header("Content-Type", "video/mp2t")
        self.send_header("Content-Length", str(len(data)))
        self.end_headers()
        self.wfile.write(data)

    def do_HEAD(self):
        self.server.requests.append(self.path)
        self.send_response(200)
        self.send_header("Content-Length", "16")
        self.end_headers()

    def log_message(self, *_):
        pass


class GatewayTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.upstream = ThreadingHTTPServer(("127.0.0.1", 0), Upstream)
        cls.upstream.requests = []
        cls.gateway = proxy.serve("ivanmalison@gmail.com", 0, cls.upstream.server_port)
        for server in (cls.upstream, cls.gateway):
            threading.Thread(target=server.serve_forever, daemon=True).start()

    @classmethod
    def tearDownClass(cls):
        for server in (cls.gateway, cls.upstream):
            server.shutdown()
            server.server_close()

    def request(self, path="/camera/index.m3u8", identities=(), method="GET"):
        connection = http.client.HTTPConnection("127.0.0.1", self.gateway.server_port)
        connection.putrequest(method, path)
        for identity in identities:
            connection.putheader("Tailscale-User-Login", identity)
        connection.endheaders()
        response = connection.getresponse()
        result = response.status, response.read(), dict(response.getheaders())
        connection.close()
        return result

    def test_missing_wrong_and_duplicate_identities_never_reach_media(self):
        count = len(self.upstream.requests)
        for identities in (
            (),
            ("alexmalison@gmail.com",),
            ("ivanmalison@gmail.com.evil",),
            ("ivanmalison@gmail.com", "alexmalison@gmail.com"),
            ("ivanmalison@gmail.com", "ivanmalison@gmail.com"),
            ("ivanmalison@gmail.com, alexmalison@gmail.com",),
        ):
            with self.subTest(identities=identities):
                self.assertEqual(self.request(identities=identities)[0], 403)
        self.assertEqual(len(self.upstream.requests), count)

    def test_authorized_segments_and_playlists_keep_bytes_and_queries(self):
        path = "/camera/segment.ts?sequence=42"
        status, body, headers = self.request(path, ("ivanmalison@gmail.com",))
        self.assertEqual((status, body), (200, b"test-media-bytes"))
        self.assertEqual(self.upstream.requests[-1], path)
        self.assertEqual(headers["Cache-Control"], "no-store")

    def test_head(self):
        status, body, _ = self.request(
            identities=("ivanmalison@gmail.com",), method="HEAD"
        )
        self.assertEqual((status, body), (200, b""))

    def test_only_camera_path_is_proxied(self):
        for path in (
            "/v3/config/get",
            "//example.org/camera/",
            "http://example.org/camera/",
            "/camera-other/",
        ):
            self.assertEqual(self.request(path, ("ivanmalison@gmail.com",))[0], 404)

    def test_root_requires_identity(self):
        self.assertEqual(self.request("/")[0], 403)
        status, _, headers = self.request("/", ("ivanmalison@gmail.com",))
        self.assertEqual((status, headers["Location"]), (302, "/camera/"))


class ProfileTests(unittest.TestCase):
    def test_start_preserves_an_active_stream(self):
        processes = [(42, "OBS --collection TailnetCamera --startstreaming")]
        with tempfile.TemporaryDirectory() as directory:
            paused = Path(directory) / "paused"
            paused.touch()
            with (
                patch.object(capture, "publishing", return_value=True),
                patch.object(capture.os, "kill") as kill,
                patch.object(capture.subprocess, "run") as run,
            ):
                capture.start_capture(paused, processes)
            kill.assert_not_called()
            run.assert_not_called()
            self.assertFalse(paused.exists())

    def test_start_does_not_touch_an_unmanaged_obs_session(self):
        with tempfile.TemporaryDirectory() as directory:
            paused = Path(directory) / "paused"
            paused.touch()
            with (
                patch.object(capture.subprocess, "run") as run,
                self.assertRaisesRegex(RuntimeError, "another collection"),
            ):
                capture.start_capture(paused, [(42, "OBS --collection Other")])
            run.assert_not_called()
            self.assertTrue(paused.exists())

    def test_start_restarts_the_managed_profile_when_it_is_not_publishing(self):
        with tempfile.TemporaryDirectory() as directory:
            paused = Path(directory) / "paused"
            with (
                patch.object(capture, "publishing", return_value=False),
                patch.object(capture.os, "kill") as kill,
                patch.object(capture.subprocess, "run") as run,
            ):
                capture.start_capture(paused, [(42, "OBS --collection TailnetCamera")])
            kill.assert_called_once()
            run.assert_called_once()
            self.assertFalse(paused.exists())

    def test_unattended_restart_clears_only_empty_obs_run_markers(self):
        with tempfile.TemporaryDirectory() as directory:
            home = Path(directory)
            markers = home / "Library/Application Support/obs-studio/.sentinel"
            markers.mkdir(parents=True)
            (markers / "run_previous-session").touch()
            (markers / "run_nonempty").write_text("preserve")
            (markers / "other").touch()
            capture.clear_run_markers(home)
            self.assertFalse((markers / "run_previous-session").exists())
            self.assertEqual((markers / "run_nonempty").read_text(), "preserve")
            self.assertTrue((markers / "other").exists())

    def test_pause_only_targets_the_exact_managed_collection(self):
        self.assertTrue(
            capture.managed("OBS --collection TailnetCamera --startstreaming")
        )
        self.assertFalse(capture.managed("OBS --collection TailnetCamera-other"))
        self.assertFalse(
            capture.managed("OBS --profile TailnetCamera --collection Other")
        )

    def test_absent_hardware_does_not_start_obs(self):
        with tempfile.TemporaryDirectory() as home:
            self.assertFalse(capture.prepare(Path(home), {}))
            self.assertFalse((Path(home) / "Library").exists())

    def test_only_camera_and_microphone_sources_and_loopback_destination(self):
        inventory = {
            "SPCameraDataType": [{"spcamera_unique-id": "camera-uuid"}],
            "SPAudioDataType": [{"_items": [{"coreaudio_device_input": 1}]}],
        }
        with tempfile.TemporaryDirectory() as directory:
            home = Path(directory)
            root = home / "Library/Application Support/obs-studio"
            unrelated = root / "basic/profiles/Existing/basic.ini"
            unrelated.parent.mkdir(parents=True)
            unrelated.write_text("existing profile")
            self.assertTrue(capture.prepare(home, inventory))
            scenes = json.loads((root / "basic/scenes/TailnetCamera.json").read_text())
            self.assertEqual(
                [s["id"] for s in scenes["sources"]],
                ["macos-avcapture", "coreaudio_input_capture", "scene"],
            )
            self.assertEqual(scenes["sources"][0]["settings"]["device"], "camera-uuid")
            destination = json.loads(
                (root / "basic/profiles/TailnetCamera/service.json").read_text()
            )
            self.assertEqual(
                destination["settings"]["server"], "rtmp://127.0.0.1:18379"
            )
            self.assertEqual(unrelated.read_text(), "existing profile")

    def test_audio_only_and_video_only(self):
        for camera_id, microphone, ids in (
            (None, True, ["coreaudio_input_capture", "scene"]),
            ("camera", False, ["macos-avcapture", "scene"]),
        ):
            self.assertEqual(
                [s["id"] for s in capture.collection(camera_id, microphone)["sources"]],
                ids,
            )


if __name__ == "__main__":
    unittest.main()
