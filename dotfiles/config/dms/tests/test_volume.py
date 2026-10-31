import os
import subprocess
import tempfile
import unittest
from pathlib import Path

functions = Path(__file__).resolve().parents[3] / "lib/functions"


class VolumeTests(unittest.TestCase):
    def setUp(self):
        self.directory = tempfile.TemporaryDirectory()
        self.addCleanup(self.directory.cleanup)
        self.root = Path(self.directory.name)
        self.log = self.root / "commands"
        self.env = dict(
            os.environ,
            PATH=f"{self.root}:{os.environ['PATH']}",
            COMMAND_LOG=str(self.log),
            SHELL_UI="dms",
            DMS_EXIT="0",
        )
        for name, body in {
            "desktop_shell_ui": 'printf "%s\\n" "$SHELL_UI"',
            "dms": 'printf "dms %s\\n" "$*" >> "$COMMAND_LOG"; exit "$DMS_EXIT"',
            "pulsemixer": 'printf "pulsemixer %s\\n" "$*" >> "$COMMAND_LOG"',
        }.items():
            command = self.root / name
            command.write_text(f"#!/bin/sh\n{body}\n")
            command.chmod(0o755)

    def run_volume(self, *args):
        self.log.write_text("")
        result = subprocess.run(
            [
                "zsh",
                "-f",
                "-c",
                'fpath=("$1" $fpath); shift; autoload set_volume; set_volume "$@"',
                "test-volume",
                str(functions),
                *args,
            ],
            env=self.env,
            capture_output=True,
            text=True,
            check=False,
        )
        return result, self.log.read_text().splitlines()

    def test_dms_volume_and_mute_use_native_controls(self):
        for args, expected in [
            (("--unmute", "--change-volume", "+5"), "dms ipc call audio increment 5"),
            (("--unmute", "--change-volume", "-5"), "dms ipc call audio decrement 5"),
            (("--toggle-mute",), "dms ipc call audio mute"),
        ]:
            with self.subTest(args=args):
                result, commands = self.run_volume(*args)
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertEqual(commands, [expected])

    def test_taffybar_keeps_pulsemixer(self):
        self.env["SHELL_UI"] = "taffybar"
        result, commands = self.run_volume("--unmute", "--change-volume", "+5")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(commands, ["pulsemixer --unmute --change-volume +5"])

    def test_other_pulsemixer_options_work_in_dms(self):
        result, commands = self.run_volume("--set-volume", "42")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(commands, ["pulsemixer --set-volume 42"])

    def test_ipc_failure_is_returned_without_applying_volume_twice(self):
        self.env["DMS_EXIT"] = "7"
        result, commands = self.run_volume("--unmute", "--change-volume", "+5")
        self.assertEqual(result.returncode, 7, result.stderr)
        self.assertEqual(commands, ["dms ipc call audio increment 5"])


if __name__ == "__main__":
    unittest.main()
