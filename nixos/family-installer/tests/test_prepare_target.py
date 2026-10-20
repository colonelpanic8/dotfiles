import json
import os
from pathlib import Path
import subprocess
import tempfile
import unittest

from prepare_target import finish_target, prepare_target


class PrepareTargetTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.base = Path(self.temp.name)
        self.source = self.base / "installer"
        self.source.mkdir()
        (self.source / "flake.lock").write_text(
            '{"nodes": {"root": {}}, "root": "root", "version": 7}\n'
        )
        (self.source / "flake.nix").write_text(
            '{\n inputs = {};\n outputs = _: { value = "ready"; };\n}\n'
        )
        (self.source / ".git").mkdir()
        (self.source / ".git/config").write_text("do not copy git state")
        self.root = self.base / "target"
        generated = self.root / "etc/nixos"
        generated.mkdir(parents=True)
        self.configuration = "{...}: { imports = [./hardware-configuration.nix]; }\n"
        (generated / "configuration.nix").write_text(self.configuration)
        (generated / "hardware-configuration.nix").write_text("{...}: {}\n")

    def prepare(self, username="dad", hostname="dad-laptop", owner="other"):
        return prepare_target(self.source, self.root, username, hostname, owner)

    def test_other_primary_user_and_boot_configuration_are_preserved(self):
        flake = self.prepare()
        checkout = self.root / "srv/nixos-family-installer"
        machine = checkout / "machines/dad-laptop"
        self.assertEqual(flake, f"{checkout}#dad-laptop")
        self.assertEqual((machine / "configuration.nix").read_text(), self.configuration)
        self.assertEqual(
            json.loads((machine / "identity.json").read_text()),
            {"primaryUser": "dad", "hostname": "dad-laptop", "owner": "other"},
        )
        self.assertFalse((checkout / ".git").exists())

    def test_enrollment_contains_public_key_and_no_secret_grants(self):
        self.prepare()
        key = self.root / "etc/ssh/ssh_host_ed25519_key"
        public = subprocess.check_output(["ssh-keygen", "-y", "-f", str(key)], text=True).strip()
        request = json.loads(
            (self.root / "var/lib/nixos-family-installer/enrollment.json").read_text()
        )
        self.assertEqual(request["hostPublicKey"], public)
        self.assertEqual(request["requestedSecrets"], [])
        self.assertNotIn("PRIVATE KEY", json.dumps(request))
        self.assertEqual(key.stat().st_mode & 0o777, 0o600)

    def test_installed_flake_url_survives_metadata_roundtrip(self):
        self.prepare()
        checkout = self.root / "srv/nixos-family-installer"
        env = {
            **os.environ,
            "HOME": str(self.base / "home"),
            "XDG_CACHE_HOME": str(self.base / "cache"),
            "NIX_REMOTE": f"local?root={self.base}/store",
            "NIX_CONFIG": "experimental-features = nix-command flakes\n",
        }
        metadata = json.loads(
            subprocess.check_output(
                ["nix", "flake", "metadata", "--json", str(checkout)], env=env, text=True
            )
        )
        value = subprocess.check_output(
            ["nix", "eval", "--raw", metadata["url"] + "#value"], env=env, text=True
        )
        self.assertEqual(value, "ready")

    def test_retry_keeps_permanent_host_identity(self):
        self.prepare()
        key = self.root / "etc/ssh/ssh_host_ed25519_key"
        original = key.read_bytes()
        self.prepare()
        self.assertEqual(key.read_bytes(), original)

    def test_ivan_can_choose_a_different_login_name(self):
        self.prepare(username="ivan", owner="ivan")
        request = json.loads(
            (self.root / "var/lib/nixos-family-installer/enrollment.json").read_text()
        )
        self.assertEqual(request["primaryUser"], "ivan")
        self.assertEqual(request["owner"], "ivan")

    def test_readonly_release_source_becomes_an_editable_target(self):
        (self.source / "flake.nix").chmod(0o444)
        self.source.chmod(0o555)
        self.prepare()
        self.assertTrue((self.root / "srv/nixos-family-installer").stat().st_mode & 0o200)

    def test_finished_install_uses_the_flake_as_default_configuration(self):
        self.prepare()
        finish_target(self.root)
        finish_target(self.root)
        self.assertEqual(
            (self.root / "etc/nixos").resolve(), self.root / "srv/nixos-family-installer"
        )
        self.assertEqual(
            (self.root / "etc/nixos.calamares/configuration.nix").read_text(), self.configuration
        )

    def test_finish_does_not_replace_an_unprepared_configuration(self):
        with self.assertRaises(ValueError):
            finish_target(self.root)
        self.assertEqual(
            (self.root / "etc/nixos/configuration.nix").read_text(), self.configuration
        )

    def test_invalid_identity_is_rejected_before_writing_target(self):
        for args in (
            {"username": "root"},
            {"username": "../escape"},
            {"hostname": "../escape"},
            {"hostname": "trailing-hyphen-"},
            {"hostname": "installer"},
            {"owner": "unknown"},
        ):
            with self.subTest(args=args), self.assertRaises(ValueError):
                self.prepare(**args)
        self.assertFalse((self.root / "srv").exists())

    def test_accepts_valid_mixed_case_computer_names(self):
        self.prepare(hostname="Dads-PC2")
        self.assertTrue((self.root / "srv/nixos-family-installer/machines/Dads-PC2").is_dir())

    def test_refuses_to_overwrite_an_existing_checkout(self):
        checkout = self.root / "srv/nixos-family-installer"
        checkout.mkdir(parents=True)
        (checkout / "flake.nix").write_text("existing work")
        with self.assertRaisesRegex(ValueError, "existing configuration"):
            self.prepare()
        self.assertEqual((checkout / "flake.nix").read_text(), "existing work")


if __name__ == "__main__":
    unittest.main()
