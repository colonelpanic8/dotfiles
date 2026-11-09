# Sync pass to Vaultwarden

`pass-vaultwarden-sync` previews changes. `pass-vaultwarden-sync --apply`
updates the personal vault on jimi-hendnix using the master password in
`jimi-hendnix.taileb3aad.ts.net/vaultwarden`.

The sync copies passwords, usernames, URLs, TOTP URIs, and metadata from
`~/.password-store`, including multiline YAML literal fields. Existing matching
items keep their IDs, names, folders, archive state,
attachments, passkeys, and unrelated fields and notes. A `pass-sync-path`
custom field identifies each managed entry on subsequent runs. Ambiguous
matches stop the run before writes. Temporary entries ending in `-tmp` are
excluded. Entries missing from pass are not deleted.

An item carrying the marker matches whether it is live, archived, trashed, or
in an organization. Archived personal items are updated in place and stay
archived. Trashed and organization items are left untouched and are not
recreated. Unmarked items are matched by name only while they are live,
personal logins.

Matched items are updated according to their type. The first line of the pass
entry becomes a login's password, a card's number (with `expiration` or
`Expiration Date`, `security-code`, and a brand derived from the number), an
identity's SSN, passport, or license number (chosen by pass path in
`IDENTITY_FIELDS`), or a secure note's hidden `value` field. Other keys become
custom fields and notes metadata. Items of other types, and identities without
a path mapping, are skipped. New entries are created as logins, except those
under `cards/`, which become cards.

When copying structured metadata would overflow the CLI's notes limit, large
values remain complete in custom fields and are omitted from the notes copy.
The notes budget accounts for UTF-8 and encryption overhead. Existing notes
and freeform source text are preserved.

Writes use four workers with separate CLI profiles. A process lock prevents
overlapping sync runs. Use `--jobs 1` for serial writes or `--only ENTRY` to
refresh one entry.

Before writing changes, the helper saves an account-encrypted personal-vault
export in `~/.local/state/pass-vaultwarden-sync/backups/`. It reads back the
vault after writing and checks every source entry. Decrypted source data and
the login password stay in process memory. An isolated CLI profile lives in
the user runtime directory and is removed after logout.

Vaultwarden 1.37.3 lacks a key-ID endpoint used by CLI 2026.9.0. The module
obtains CLI 2026.7.0 from a hash-pinned nixpkgs revision in
`nixos/packages/bitwarden-cli-vaultwarden/default.nix`.

The installed sync command packages the Python script and its runtime
dependencies in the Nix store. To build it independently:

```sh
cd /srv/dotfiles/nixos
nix build --impure --no-write-lock-file --no-link \
  .#nixosConfigurations.jimi-hendnix.config.system.build.pass-vaultwarden-sync
```

The NixOS module `nixos/vaultwarden-jimi.nix` is imported only by jimi-hendnix
and asserts that hostname. It installs the command and schedules a one-way sync
every 15 minutes. The service uses the existing agenix GPG passphrase file to decrypt
without a prompt. Manual runs use the GPG agent, or the file named by
`PASS_SYNC_GPG_PASSPHRASE_FILE` when set. Failed runs leave source entries
intact and retry on the next tick.

```sh
systemctl --user start pass-vaultwarden-sync.service
systemctl --user status pass-vaultwarden-sync.timer
journalctl --user -u pass-vaultwarden-sync.service -n 30
pass-vaultwarden-sync --audit-sizes
```

Edits made directly in Vaultwarden do not flow back to pass. Managed values
are refreshed from pass on the next run. Vault-only entries are preserved.
