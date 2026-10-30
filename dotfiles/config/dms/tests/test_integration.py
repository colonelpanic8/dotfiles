import importlib.util
import os
import subprocess
import tempfile
import unittest
from pathlib import Path

spec = importlib.util.spec_from_file_location(
    "integration", Path(__file__).parents[1] / "integration.py"
)
integration = importlib.util.module_from_spec(spec)
spec.loader.exec_module(integration)
shell = Path(__file__).parents[3] / "lib/bin/desktop_shell_ui"


class IntegrationTests(unittest.TestCase):
    def test_priority_edits_preserve_taffybar_policy(self):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "priorities.yaml"
            path.write_text(
                "visibility_threshold: 2\nmax_visible_icons: 4\npriorities:\n- key: item-id:test\n  priority: 1\n"
            )
            integration.adjust_priority(path, "item-id:test", 1)
            policy, priorities = integration.read_policy(path)
            self.assertEqual(priorities["item-id:test"], 2)
            self.assertEqual(policy["visibility_threshold"], 2)
            self.assertEqual(policy["max_visible_icons"], 4)
            integration.adjust_priority(path, "item-id:test", 20)
            self.assertEqual(integration.read_policy(path)[1]["item-id:test"], 10)
            self.assertIn("- key: item-id:test\n  priority: 10\n", path.read_text())
            integration.set_threshold(path, "none")
            policy, priorities = integration.read_policy(path)
            self.assertIsNone(policy["visibility_threshold"])
            self.assertEqual(priorities["item-id:test"], 10)
            integration.set_threshold(path, "-12")
            self.assertEqual(
                integration.read_policy(path)[0]["visibility_threshold"], -10
            )

    def test_shell_failure_restores_taffybar_and_session_fallback(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            state = root / "state"
            command = root / "systemctl"
            command.write_text(
                '#!/bin/sh\nprintf "%s\\n" "$*" >> "$COMMAND_LOG"\ncase "$*" in *"start dms.service") exit 1;; esac\n'
            )
            command.chmod(0o755)
            env = dict(
                os.environ,
                PATH=f"{root}:{os.environ['PATH']}",
                COMMAND_LOG=str(root / "commands"),
                IM_DESKTOP_SHELL_UI_STATE=str(state),
                XDG_SESSION_TYPE="wayland",
                XDG_CURRENT_DESKTOP="Hyprland",
                HYPRLAND_INSTANCE_SIGNATURE="test",
            )
            result = subprocess.run(
                ["bash", str(shell), "set", "dms"],
                env=env,
                capture_output=True,
                check=False,
            )
            self.assertNotEqual(result.returncode, 0)
            self.assertEqual(state.read_text().strip(), "taffybar")
            self.assertIn("start taffybar.service", (root / "commands").read_text())
            state.write_text("dms\n")
            env["XDG_SESSION_TYPE"] = "x11"
            result = subprocess.check_output(
                ["bash", str(shell), "current"], env=env, text=True
            )
            self.assertEqual(result.strip(), "taffybar")
            self.assertEqual(state.read_text().strip(), "dms")


if __name__ == "__main__":
    unittest.main()
