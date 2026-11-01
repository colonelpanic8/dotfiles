#!/usr/bin/env python3
"""Exercise the enrollment boundary without a daemon, network, or real secret."""

import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest


SCRIPT = Path(__file__).resolve().parents[2] / "nix-shared/tailscale/enroll.sh"


class EnrollmentTest(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.calls = self.root / "calls"
        self.key = self.root / "authkey"
        self.key.write_text("FAKE_CREDENTIAL_MUST_NOT_APPEAR_IN_OUTPUT\n")
        tailscale = self.root / "tailscale"
        tailscale.write_text(
            "#!/usr/bin/env python3\n"
            "import json, os, sys\n"
            "with open(os.environ['CALLS'], 'a') as f:\n"
            "    f.write(json.dumps(sys.argv[1:]) + '\\n')\n"
            "if sys.argv[1] == 'status':\n"
            "    print(os.environ['STATUS'])\n"
            "    sys.exit(int(os.environ.get('STATUS_RC', '0')))\n"
            "print('FAKE_CREDENTIAL_MUST_NOT_APPEAR_IN_OUTPUT')\n"
            "sys.exit(int(os.environ.get('UP_RC', '0')))\n"
        )
        tailscale.chmod(0o700)
        stat = self.root / "stat"
        stat.write_text('#!/usr/bin/env bash\nprintf "%s\\n" "$FILE_METADATA"\n')
        stat.chmod(0o700)
        # Keep expiry tests deterministic and independent of BSD/GNU date on
        # the test host. Production always receives GNU date from Nix.
        date = self.root / "date"
        date.write_text(
            "#!/usr/bin/env python3\n"
            "import datetime, sys\n"
            "if sys.argv[1] == '-d':\n"
            "    print(int(datetime.datetime.fromisoformat(sys.argv[2]).timestamp()))\n"
            "else:\n"
            "    print(1800000000)\n"
        )
        date.chmod(0o700)
        self.env = {
            **os.environ,
            "TAILSCALE": str(tailscale),
            "JQ": shutil.which("jq"),
            "STAT": str(stat),
            "DATE": str(date),
            "REAUTHENTICATE_EXPIRED": "false",
            "CALLS": str(self.calls),
            "AUTH_KEY_FILE": str(self.key),
            "NODE_HOSTNAME": "msi-elliott",
            "OPERATOR": "alex",
            "ACCEPT_DNS": "true",
            "ADVERTISE_TAGS": "",
            "FILE_METADATA": "0 600",
        }

    def run_enrollment(self, status, **env):
        self.calls.unlink(missing_ok=True)
        result = subprocess.run(
            ["bash", str(SCRIPT)],
            env={**self.env, "STATUS": json.dumps(status), **env},
            capture_output=True,
            text=True,
            check=False,
        )
        self.assertNotIn("FAKE_CREDENTIAL", result.stdout + result.stderr)
        calls = [json.loads(line) for line in self.calls.read_text().splitlines()]
        return result, [call for call in calls if call[0] == "up"]

    def assert_unchanged(self, status, **env):
        result, up = self.run_enrollment(status, **env)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(up, [])

    def test_running_needs_no_credential_even_if_expiry_is_in_the_past(self):
        self.key.unlink()
        self.assert_unchanged({"BackendState": "Running", "Self": {"KeyExpiry": "2000-01-01"}})

    def test_persisted_node_key_without_netmap_is_not_reenrolled(self):
        self.assert_unchanged({
            "BackendState": "NeedsLogin", "HaveNodeKey": True,
            "Self": None, "CurrentTailnet": None, "TailscaleIPs": None,
        })

    def test_malformed_status_fields_do_not_authorize_enrollment(self):
        for field in ["HaveNodeKey", "Self", "CurrentTailnet", "TailscaleIPs"]:
            with self.subTest(field=field):
                value = "false" if field == "HaveNodeKey" else False
                self.assert_unchanged({"BackendState": "NeedsLogin", field: value})

    def test_existing_needs_login_is_not_reenrolled(self):
        self.assert_unchanged({"BackendState": "NeedsLogin", "Self": {"ID": "existing-node"}})

    def test_existing_tailnet_is_not_replaced(self):
        self.assert_unchanged({"BackendState": "NeedsLogin", "CurrentTailnet": {"Name": "existing"}})

    def test_existing_ip_is_not_replaced(self):
        self.assert_unchanged({"BackendState": "NeedsLogin", "TailscaleIPs": ["100.100.100.100"]})

    def test_existing_self_ip_is_not_replaced(self):
        self.assert_unchanged({"BackendState": "NeedsLogin", "Self": {"TailscaleIPs": ["100.100.100.100"]}})

    def test_ambiguous_login_status_does_not_enroll(self):
        self.assert_unchanged({"BackendState": "NeedsLogin", "Self": []})

    def test_stopped_node_is_left_stopped(self):
        self.assert_unchanged({"BackendState": "Stopped"})

    def test_machine_approval_is_not_another_enrollment(self):
        self.assert_unchanged({"BackendState": "NeedsMachineAuth"})

    def test_invalid_status_does_not_enroll(self):
        self.assert_unchanged(None)

    def test_starting_node_waits_for_a_later_attempt(self):
        self.assert_unchanged({"BackendState": "Starting"})

    def test_missing_key_does_not_fail_boot(self):
        self.key.unlink()
        self.assert_unchanged({"BackendState": "NeedsLogin"})

    def test_new_node_enrolls_with_a_file_reference(self):
        result, up = self.run_enrollment(
            {"BackendState": "NeedsLogin"},
            STATUS_RC="1",
            ADVERTISE_TAGS="tag:alex-pc",
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(len(up), 1)
        self.assertEqual(up[0], [
            "up", "--auth-key", f"file:{self.key}", "--hostname", "msi-elliott",
            "--accept-dns=true", "--timeout=30s", "--operator=alex", "--advertise-tags=tag:alex-pc",
        ])

    def test_unsafe_permissions_refuse_enrollment(self):
        result, up = self.run_enrollment({"BackendState": "NeedsLogin"}, FILE_METADATA="0 640")
        self.assertEqual(result.returncode, 1)
        self.assertEqual(up, [])

    def test_nonroot_owner_refuses_enrollment(self):
        result, up = self.run_enrollment({"BackendState": "NeedsLogin"}, FILE_METADATA="1000 600")
        self.assertEqual(result.returncode, 1)
        self.assertEqual(up, [])

    def test_disabled_credential_skips_enrollment(self):
        self.key.write_text("DISABLED\n")
        self.assert_unchanged({"BackendState": "NeedsLogin"})

    def test_empty_key_skips_enrollment(self):
        self.key.write_text("")
        self.assert_unchanged({"BackendState": "NeedsLogin"})

    def test_alternate_profile_options(self):
        result, up = self.run_enrollment(
            {"BackendState": "NeedsLogin"},
            OPERATOR="", ACCEPT_DNS="false", NODE_HOSTNAME="another-pc",
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(up, [[
            "up", "--auth-key", f"file:{self.key}", "--hostname", "another-pc",
            "--accept-dns=false", "--timeout=30s",
        ]])

    def test_expired_running_node_reauthenticates_only_when_enabled(self):
        result, up = self.run_enrollment(
            {"BackendState": "Running", "Self": {"ID": "old", "KeyExpiry": "2000-01-01"}},
            REAUTHENTICATE_EXPIRED="true",
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(len(up), 1)
        self.assertIn("--force-reauth", up[0])

    def test_expired_needs_login_can_be_repaired(self):
        result, up = self.run_enrollment(
            {"BackendState": "NeedsLogin", "Self": {"ID": "old", "KeyExpiry": "2000-01-01"}},
            REAUTHENTICATE_EXPIRED="true",
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(len(up), 1)
        self.assertIn("--force-reauth", up[0])

    def test_repair_policy_does_not_resume_stopped_or_approval_pending_node(self):
        for state in ["Stopped", "NeedsMachineAuth"]:
            with self.subTest(state=state):
                self.assert_unchanged(
                    {"BackendState": state, "Self": {"KeyExpiry": "2000-01-01"}},
                    REAUTHENTICATE_EXPIRED="true",
                )

    def test_unexpired_or_unknown_expiry_does_not_reauthenticate(self):
        for expiry in [None, "invalid", "2999-01-01"]:
            with self.subTest(expiry=expiry):
                self.assert_unchanged(
                    {"BackendState": "Running", "Self": {"KeyExpiry": expiry}},
                    REAUTHENTICATE_EXPIRED="true",
                )

    def test_repair_without_verified_expiry_does_not_replace_existing_node(self):
        self.assert_unchanged(
            {"BackendState": "NeedsLogin", "Self": {"ID": "old"}},
            REAUTHENTICATE_EXPIRED="true",
        )

    def test_missing_key_during_expiry_repair_does_not_fail_boot(self):
        self.key.unlink()
        self.assert_unchanged(
            {"BackendState": "Running", "Self": {"KeyExpiry": "2000-01-01"}},
            REAUTHENTICATE_EXPIRED="true",
        )

    def test_failed_enrollment_does_not_leak_command_output(self):
        result, up = self.run_enrollment({"BackendState": "NeedsLogin"}, UP_RC="1")
        self.assertEqual(result.returncode, 1)
        self.assertEqual(len(up), 1)


if __name__ == "__main__":
    unittest.main()
