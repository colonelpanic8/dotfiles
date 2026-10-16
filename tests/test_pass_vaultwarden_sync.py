import copy
import runpy
import unittest
from pathlib import Path

SYNC = runpy.run_path(
    str(
        Path(__file__).resolve().parents[1]
        / "dotfiles/lib/bin/pass-vaultwarden-sync.py"
    )
)


def source(path="example.com/alice", contents="new-secret\nuser: alice\n"):
    return SYNC["source_entry"](path, contents)


def item(identifier="one", username="alice", **changes):
    return {
        "id": identifier,
        "name": "example.com",
        "type": 1,
        "login": {"username": username, "password": "old-secret"},
        "notes": "A note from the vault",
        "fields": [],
        **changes,
    }


class SyncTest(unittest.TestCase):
    def test_refresh_preserves_vault_data_and_is_idempotent(self):
        old = item(fields=[{"name": "security-code", "value": "old", "type": 1}])
        before = copy.deepcopy(old)
        src = source(
            contents="new-secret\nuser: alice\nsecurity-code: new\nurl: https://example.com\notpauth://totp/example?secret=EXAMPLE\n"
        )
        new = SYNC["desired_item"](src, old)
        self.assertEqual(old, before)
        self.assertEqual(new["login"]["password"], "new-secret")
        self.assertEqual(new["login"]["username"], "alice")
        self.assertTrue(new["login"]["totp"].startswith("otpauth://"))
        self.assertEqual(new["fields"][0]["value"], "new")
        self.assertTrue(new["notes"].startswith(old["notes"]))
        self.assertEqual(SYNC["desired_item"](src, new), new)

    def test_unknown_login_does_not_replace_existing_username(self):
        src = source("example.com/unknown-login", "secret\n")
        self.assertEqual(
            SYNC["desired_item"](src, item())["login"]["username"], "alice"
        )

    def test_api_omitting_empty_notes_does_not_cause_repeat_updates(self):
        src = source(contents="secret\n")
        stored = SYNC["desired_item"](src)
        stored.pop("notes")
        self.assertEqual(SYNC["desired_item"](src, stored), stored)

    def test_explicit_empty_username_clears_the_existing_value(self):
        src = source(contents="secret\nuser:\n")
        stored = SYNC["desired_item"](src, item())
        self.assertNotIn("username", stored["login"])
        self.assertEqual(SYNC["desired_item"](src, stored), stored)

    def test_long_metadata_stays_complete_without_overflowing_notes(self):
        blob = "a" * 5104
        src = source(
            "signing/release", "secret\nkeystore-base64: " + blob + "\ncreated: today\n"
        )
        old = item(notes="b" * 5145)
        new = SYNC["desired_item"](src, old)
        self.assertLessEqual(len(new["notes"]), 10000)
        self.assertTrue(new["notes"].startswith(old["notes"]))
        self.assertIn(
            {"name": "keystore-base64", "value": blob, "type": 1}, new["fields"]
        )
        self.assertEqual(SYNC["desired_item"](src, new), new)

    def test_yaml_literal_keystore_is_preserved_as_one_complete_field(self):
        blob = ("a" * 76 + "\n") * 76
        yaml_blob = "".join("  " + line + "\n" for line in blob.splitlines())
        src = source(
            "signing/release",
            "secret\nstorepass: example\nkeystore_base64: |\n" + yaml_blob,
        )
        self.assertEqual(src["fields"]["keystore_base64"], blob)
        new = SYNC["desired_item"](src, item(notes="Existing notes " + "b" * 5800))
        self.assertLessEqual(len(new["notes"]), 10000)
        self.assertIn(
            {"name": "keystore_base64", "value": blob, "type": 1}, new["fields"]
        )
        self.assertEqual(SYNC["desired_item"](src, new), new)

    def test_note_budget_accounts_for_encryption_and_utf8(self):
        blob = "a" * 4000
        src = source("signing/release", "secret\nkeystore-base64: " + blob + "\n")
        new = SYNC["desired_item"](src, item(notes="é" * 2250))
        self.assertLessEqual(len(new["notes"].encode("utf-8")), 7400)
        self.assertIn(
            {"name": "keystore-base64", "value": blob, "type": 1}, new["fields"]
        )

    def test_duplicate_names_resolve_by_username(self):
        alice, bob = item(), item("two", "bob")
        matches = SYNC["match_entries"]([source()], [alice, bob], {})
        self.assertEqual(matches["example.com/alice"]["id"], "one")

    def test_ambiguous_names_fail_before_writes(self):
        with self.assertRaisesRegex(RuntimeError, "Ambiguous"):
            SYNC["match_entries"]([source()], [item(), item("two")], {})

    def test_two_source_entries_cannot_overwrite_one_item(self):
        matches = SYNC["match_entries"](
            [source(), source("example.com/bob", "secret\nuser: bob\n")], [item()], {}
        )
        self.assertEqual(set(matches), {"example.com/alice"})

    def test_marker_survives_vault_renames_and_ignores_organizations(self):
        managed = item(
            name="Renamed",
            fields=[
                {"name": "pass-sync-path", "value": "example.com/alice", "type": 0}
            ],
        )
        organization = item("org", organizationId="organization")
        matches = SYNC["match_entries"]([source()], [managed, organization], {})
        self.assertEqual(matches["example.com/alice"]["id"], "one")

    def test_new_entry_contains_password_and_full_original_metadata(self):
        src = source(contents="secret\nuser: alice\nfreeform note\nkey: value\n")
        new = SYNC["desired_item"](src)
        self.assertEqual(new["name"], "example.com/alice")
        self.assertEqual(new["login"]["password"], "secret")
        self.assertIn("freeform note", new["notes"])
        self.assertIn({"name": "key", "value": "value", "type": 1}, new["fields"])

    def test_generic_usernames_and_tokens_do_not_match_other_sites(self):
        unrelated = item(name="token", folderId="rocket")
        matches = SYNC["match_entries"](
            [source("github.com/token", "new-token\n")],
            [unrelated],
            {"rocket": "rocket-sense"},
        )
        self.assertEqual(matches, {})

    def test_legacy_domain_alias_updates_the_existing_account(self):
        matches = SYNC["match_entries"](
            [source("airbnb.com/alice")], [item(name="airbnb")], {}
        )
        self.assertEqual(matches["airbnb.com/alice"]["id"], "one")

    def test_folder_distinguishes_repeated_generic_names(self):
        matches = SYNC["match_entries"](
            [source("github.com/token", "new-token\n")],
            [
                item(name="token", folderId="github"),
                item("two", name="token", folderId="rocket"),
            ],
            {"github": "github", "rocket": "rocket-sense"},
        )
        self.assertEqual(matches["github.com/token"]["id"], "one")

    def test_unique_unchanged_secret_recognizes_a_renamed_entry(self):
        existing = item(
            name="Old device name", login={"password": "unique-long-secret"}
        )
        matches = SYNC["match_entries"](
            [source("devices/new-device-name", "unique-long-secret\n")], [existing], {}
        )
        self.assertEqual(matches["devices/new-device-name"]["id"], "one")


if __name__ == "__main__":
    unittest.main()
