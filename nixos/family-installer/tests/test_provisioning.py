import base64
import json
import os
from pathlib import Path
import subprocess
import tempfile
from types import SimpleNamespace
import unittest
from unittest.mock import patch

from prepare_media import codex_credentials, inspect_usb, write_usb
from prepare_target import prepare_target
from provisioning import (
    boot_target,
    decrypt_for_host,
    encrypt_for_host,
    install_accounts,
    install_identity,
    load_live_seed,
    public_key,
    provision_target,
    refresh_target_accounts,
    unlock_live,
    validate_seed,
)


def mode(path):
    return path.stat().st_mode & 0o777


def codex_auth(email, access="fixture-access"):
    claims = base64.urlsafe_b64encode(json.dumps({"email": email}).encode()).decode().rstrip("=")
    return {
        "tokens": {
            "id_token": f"x.{claims}.x",
            "access_token": access,
            "refresh_token": "fixture-refresh",
        }
    }


class ProvisioningTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.base = Path(self.temp.name)
        self.key = self.base / "key"
        subprocess.run(
            ["ssh-keygen", "-q", "-t", "ed25519", "-N", "", "-f", str(self.key)], check=True
        )
        self.accounts = {"codex": {"tokens": {"access_token": "fixture-secret"}}}
        self.seed = {
            "version": 1,
            "username": "alex",
            "hostname": "alex-nixos",
            "hostPrivateKey": self.key.read_text(),
            "hostPublicKey": public_key(self.key),
            "authorizedKeys": [public_key(self.key)],
            "accounts": self.encrypt(self.accounts),
        }

    def encrypt(self, data):
        return encrypt_for_host(json.dumps(data).encode(), public_key(self.key))

    def user(self, home):
        home.mkdir(parents=True, exist_ok=True)
        entry = SimpleNamespace(pw_dir=str(home), pw_uid=os.getuid(), pw_gid=os.getgid())
        return patch("pwd.getpwnam", return_value=entry)

    def prepared_target(self):
        source = self.base / "source"
        source.mkdir()
        (source / "flake.lock").write_text("{}")
        (source / "flake.nix").write_text("{}")
        root = self.base / "target"
        generated = root / "etc/nixos"
        generated.mkdir(parents=True)
        for name in ("configuration.nix", "hardware-configuration.nix"):
            (generated / name).write_text("{...}: {}")
        prepare_target(source, root, "alex", "alex-nixos", "other", self.seed)
        return root

    def unlock(self, home, state, tailscale=None):
        bundle = self.base / "family-seed.age"
        bundle.write_text("passphrase-encrypted")
        ready = self.base / "family-installer.ready"

        def check_output(args, **kwargs):
            if args == ["age", "--decrypt", str(bundle)]:
                return json.dumps(self.seed).encode()
            return subprocess.check_output(args, **kwargs)

        def run(args, **kwargs):
            self.assertEqual(args[0], "tailscale")
            return tailscale(args)

        fake = SimpleNamespace(check_output=check_output, run=run)
        with self.user(home), patch("provisioning.subprocess", fake):
            unlock_live(bundle, "nixos", state=state, ready=ready)
        return bundle, ready

    def test_accounts_require_prepared_host_identity(self):
        decrypted = decrypt_for_host(self.seed["accounts"], self.key)
        self.assertEqual(json.loads(decrypted), self.accounts)
        self.assertNotIn("fixture-secret", self.seed["accounts"])
        other = self.base / "other"
        subprocess.run(
            ["ssh-keygen", "-q", "-t", "ed25519", "-N", "", "-f", str(other)], check=True
        )
        with self.assertRaises(subprocess.CalledProcessError):
            decrypt_for_host(self.seed["accounts"], other)

    def test_mismatched_user_host_or_owner_is_rejected(self):
        for user, host, owner in [
            ("ivan", "alex-nixos", "other"),
            ("alex", "wrong", "other"),
            ("alex", "alex-nixos", "ivan"),
        ]:
            with self.subTest(user=user, host=host, owner=owner), self.assertRaises(ValueError):
                validate_seed(self.seed, user, host, owner)

    def test_target_keeps_seeded_key_and_only_public_configuration(self):
        root = self.prepared_target()
        prepare_target(self.base / "source", root, "alex", "alex-nixos", "other", self.seed)
        target_key = root / "etc/ssh/ssh_host_ed25519_key"
        self.assertEqual(public_key(target_key), self.seed["hostPublicKey"])
        self.assertEqual(mode(target_key), 0o600)
        machine = root / "srv/nixos-family-installer/machines/alex-nixos"
        text = "".join(p.read_text() for p in machine.iterdir())
        self.assertNotIn("PRIVATE KEY", text)
        self.assertNotIn("fixture-secret", text)
        self.assertIn("provisioning.nix", (machine / "default.nix").read_text())
        accounts = root / "var/lib/family-installer/accounts.age"
        self.assertEqual(mode(accounts), 0o600)
        self.assertEqual(
            json.loads(decrypt_for_host(accounts.read_text(), target_key)), self.accounts
        )

    def test_existing_different_host_key_is_not_replaced(self):
        self.seed["hostPublicKey"] = "ssh-ed25519 different"
        original = self.key.read_bytes()
        with self.assertRaises(ValueError):
            install_identity(self.seed, self.key)
        self.assertEqual(original, self.key.read_bytes())

    def test_refreshed_account_is_not_overwritten(self):
        home = self.base / "home"
        home.mkdir()
        install_accounts(self.accounts, home, os.getuid(), os.getgid())
        auth = home / ".codex/auth.json"
        self.assertEqual(mode(auth), 0o600)
        self.assertEqual(mode(auth.parent), 0o700)
        self.assertFalse((home / ".claude").exists())
        auth.write_text('{"refreshed": true}')
        install_accounts(self.accounts, home, os.getuid(), os.getgid())
        self.assertEqual(json.loads(auth.read_text()), {"refreshed": True})

    def test_locked_personal_bundle_blocks_installation(self):
        bundle = self.base / "family-seed.age"
        state = self.base / "state"
        self.assertIsNone(load_live_seed(bundle, state))
        bundle.write_text("passphrase-encrypted")
        with self.assertRaisesRegex(ValueError, "Unlock"):
            load_live_seed(bundle, state)
        state.mkdir()
        (state / "seed.json").write_text(json.dumps(self.seed))
        with self.assertRaisesRegex(ValueError, "Unlock"):
            load_live_seed(bundle, state)

    def test_unlock_is_idempotent_and_private(self):
        home = self.base / "nixos"
        state = self.base / "state"
        (home / ".ssh").mkdir(parents=True, mode=0o700)
        (home / ".ssh/authorized_keys").write_text("ssh-ed25519 existing\n")
        for _ in range(2):
            _, ready = self.unlock(home, state)
        self.assertEqual(load_live_seed(self.base / "family-seed.age", state), self.seed)
        self.assertEqual(ready.read_text(), "alex-nixos\n")
        authorized = (home / ".ssh/authorized_keys").read_text().split()
        self.assertEqual(authorized.count("existing"), 1)
        self.assertEqual(authorized.count(self.seed["authorizedKeys"][0].split()[1]), 1)
        auth = home / ".codex/auth.json"
        self.assertEqual(json.loads(auth.read_text()), self.accounts["codex"])
        self.assertEqual(public_key(state / "target-host-key"), self.seed["hostPublicKey"])
        for path in (
            state,
            auth.parent,
            home / ".ssh",
        ):
            self.assertEqual(mode(path), 0o700, path)
        for path in (
            state / "target-host-key",
            state / "seed.json",
            auth,
            home / ".ssh/authorized_keys",
        ):
            self.assertEqual(mode(path), 0o600, path)

    def test_live_tailscale_key_is_removed_after_enrollment(self):
        self.seed["tailscaleLive"] = self.encrypt("tskey-live-fixture")
        state = self.base / "state"
        seen = []

        def tailscale(args):
            auth = Path(args[2].removeprefix("--auth-key=file:"))
            seen.append((args, auth.read_text(), mode(auth)))
            return subprocess.CompletedProcess(args, 0)

        self.unlock(self.base / "nixos", state, tailscale=tailscale)
        [(args, key, key_mode)] = seen
        self.assertEqual(args[:2], ["tailscale", "up"])
        self.assertEqual(args[-2:], ["--hostname", "alex-nixos-installer"])
        self.assertEqual(json.loads(key), "tskey-live-fixture")
        self.assertEqual(key_mode, 0o600)
        self.assertFalse((state / "tailscale-live-authkey").exists())

    def test_failed_live_enrollment_stays_locked_and_removes_key(self):
        self.seed["tailscaleLive"] = self.encrypt("tskey-live-fixture")
        state = self.base / "state"

        def tailscale(args):
            raise subprocess.CalledProcessError(1, args)

        with self.assertRaises(subprocess.CalledProcessError):
            self.unlock(self.base / "nixos", state, tailscale=tailscale)
        self.assertFalse((state / "tailscale-live-authkey").exists())
        with self.assertRaisesRegex(ValueError, "Unlock"):
            load_live_seed(self.base / "family-seed.age", state)

    def test_finish_hands_off_current_live_credentials_once(self):
        self.seed["tailscaleTarget"] = self.encrypt("tskey-target-fixture")
        root = self.prepared_target()
        live = self.base / "nixos"
        install_accounts(self.accounts, live, os.getuid(), os.getgid())
        refreshed = codex_auth("alex@example.com", access="rotated-after-refresh")
        (live / ".codex/auth.json").write_text(json.dumps(refreshed))
        (live / ".claude").mkdir()
        (live / ".claude/.credentials.json").write_text('{"unselected": true}')
        refresh_target_accounts(root, self.seed, home=live)

        state = root / "var/lib/family-installer"
        key = root / "etc/ssh/ssh_host_ed25519_key"
        runtime = self.base / "run"
        self.assertFalse((live / ".codex/auth.json").exists())
        provision_target(self.seed, root, root / "srv/nixos-family-installer/machines/alex-nixos")
        refresh_target_accounts(root, self.seed, home=live)
        self.assertEqual(mode(state / "accounts.age"), 0o600)
        self.assertEqual(
            json.loads(decrypt_for_host((state / "accounts.age").read_text(), key)),
            {"codex": refreshed},
        )

        home = self.base / "alex"
        with self.user(home):
            boot_target("alex", state=state, key=key, runtime=runtime)
        auth = home / ".codex/auth.json"
        self.assertEqual(json.loads(auth.read_text()), refreshed)
        self.assertEqual(mode(auth), 0o600)
        self.assertFalse((home / ".claude").exists())
        self.assertFalse((state / "accounts.age").exists())
        self.assertEqual(
            json.loads((runtime / "tailscale-authkey").read_text()), "tskey-target-fixture"
        )
        self.assertEqual(mode(runtime / "tailscale-authkey"), 0o600)

        auth.unlink()
        with self.user(home):
            boot_target("alex", state=state, key=key, runtime=runtime)
        self.assertFalse(auth.exists())

    def test_handoff_requires_live_login_and_prepared_identity(self):
        root = self.prepared_target()
        live = self.base / "nixos"
        live.mkdir()
        with self.assertRaisesRegex(ValueError, "sign in"):
            refresh_target_accounts(root, self.seed, home=live)
        install_accounts(self.accounts, live, os.getuid(), os.getgid())
        other = self.base / "other"
        subprocess.run(
            ["ssh-keygen", "-q", "-t", "ed25519", "-N", "", "-f", str(other)], check=True
        )
        (root / "etc/ssh/ssh_host_ed25519_key").write_bytes(other.read_bytes())
        with self.assertRaisesRegex(ValueError, "identity"):
            refresh_target_accounts(root, self.seed, home=live)

    def test_internal_partition_and_small_disks_are_rejected(self):
        import plistlib

        valid = {
            "WholeDisk": True,
            "Internal": False,
            "BusProtocol": "USB",
            "Writable": True,
            "TotalSize": 100,
        }
        with (
            patch("prepare_media.platform.system", return_value="Darwin"),
            patch("prepare_media.subprocess.check_output", return_value=plistlib.dumps(valid)),
        ):
            self.assertEqual(inspect_usb("/dev/disk2", 50), valid)
        for change in (
            {"Internal": True},
            {"WholeDisk": False},
            {"BusProtocol": "PCI"},
            {"TotalSize": 1},
        ):
            with (
                patch("prepare_media.platform.system", return_value="Darwin"),
                patch(
                    "prepare_media.subprocess.check_output",
                    return_value=plistlib.dumps(valid | change),
                ),
                self.assertRaises(ValueError),
            ):
                inspect_usb("/dev/disk2", 50)

    def test_usb_copy_and_verification_commands_with_regular_files(self):
        image = self.base / "installer.iso"
        image.write_bytes(b"installer fixture\n" * 1000)
        target = self.base / "usb.img"
        target.write_bytes(b"x" * (image.stat().st_size + 37))
        raw_device = "/dev/rdisk-test"
        info = {"DeviceIdentifier": "disk-test", "TotalSize": target.stat().st_size}
        disk_actions = []

        def run(*command):
            if command[0] == "sudo":
                args = list(command[1:])
                if args[0] == "dd":
                    destination = f"of={raw_device}"
                    self.assertIn(destination, args)
                    args[args.index(destination)] = f"of={target}"
                    args.append("conv=notrunc")
                else:
                    self.assertEqual(args[0], "cmp")
                    self.assertEqual(args[-1], raw_device)
                    args[-1] = target
                subprocess.run(args, check=True, capture_output=True)
            elif command[0] == "diskutil":
                disk_actions.append(command[1])
            else:
                self.assertEqual(command, ("sync",))

        with (
            patch("prepare_media.inspect_usb", return_value=info),
            patch("builtins.input", return_value="ERASE disk-test"),
            patch("prepare_media.run", side_effect=run),
        ):
            write_usb(image, "/dev/disk-test")
        self.assertEqual(target.read_bytes(), image.read_bytes() + b"x" * 37)
        self.assertEqual(disk_actions, ["unmountDisk", "eject"])

    def test_dedicated_codex_profile_is_exported(self):
        home = self.base / "codex-login"
        home.mkdir()
        auth = codex_auth("Alex@Example.com")
        (home / "auth.json").write_text(json.dumps(auth))
        with patch("prepare_media.run") as run:
            self.assertEqual(codex_credentials(home, "alex@example.com"), auth)
        [call] = run.call_args_list
        self.assertEqual(call.args[0], "codex")
        self.assertEqual(call.args[-2:], ("login", "status"))
        self.assertIn('cli_auth_credentials_store="file"', call.args)
        self.assertEqual(call.kwargs["env"]["CODEX_HOME"], str(home))

    def test_wrong_codex_account_is_rejected_before_export(self):
        home = self.base / "codex-login"
        home.mkdir()
        (home / "auth.json").write_text(json.dumps(codex_auth("someone-else@example.com")))
        with (
            patch("prepare_media.run") as run,
            self.assertRaisesRegex(ValueError, "does not match"),
        ):
            codex_credentials(home, "alexmalison@gmail.com")
        run.assert_not_called()


if __name__ == "__main__":
    unittest.main()
