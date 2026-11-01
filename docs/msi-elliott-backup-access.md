# Backup SSH for msi-elliott

The `upterm-backup` system service provides another route to Alex's SSH server
through `wss://uptermd.upterm.dev:443`. It starts at boot without a desktop login
and restarts automatically after ten seconds if it exits. It needs the normal
Internet connection and the public relay.

Access is restricted to the same 16 kanivan and three Alex public keys used for
ordinary SSH. An unrelated key was tested and rejected. The relay's published
SSH certificate authority is pinned in `nixos/assets/upterm-known-hosts`, and
the helper independently checks the PC's SSH host key inside the tunnel.

The relay uses a dedicated private identity generated on the PC under
`/var/lib/upterm-backup/`, with owner-only permissions. It requires no Tailscale
login or Ivan credentials.

## Connect from Alex's Mac

The current relay address, pinned host keys, and Nix-managed client are saved
under `~/.local/state/upterm-backup/msi-elliott/` on the Mac. Run:

```sh
upterm-backup-connect msi-elliott
```

Commands work too:

```sh
upterm-backup-connect msi-elliott 'hostname; sudo systemctl status tailscaled'
```

The helper connects to the public relay, forwards a random localhost port to
`127.0.0.1:22` on the PC, then opens normal SSH as `alex` through that forward.
Passwordless sudo works there. Exiting closes the temporary client tunnel.
The saved relay address can be used while Tailscale is unavailable.

The Mac's Upterm client is retained by the `client` Nix build-result link under
that state directory. Another helper needs an Upterm client and one of the
authorized private keys; the private key stays on the helper's machine.

## Refresh after a relay restart or reboot

Upterm's address changes when its host process restarts, including a PC reboot.
After a restart, while ordinary SSH works, update the Mac's saved address:

```sh
upterm-backup-refresh msi-elliott alex@100.127.210.52
```

This saves public session information and the PC's public SSH host key through
an authenticated SSH connection. It never copies a private key.

If Tailscale is already unavailable and the saved address has changed, someone
at the PC can run:

```sh
upterm-backup-info
```

The output contains the current direct Upterm connection command. Only the
authorized keys can use it. The system service continues running when desktop
sessions end.

## Service checks

```sh
systemctl status upterm-backup
journalctl -u upterm-backup --no-pager
```

During setup, normal SSH and sudo were verified through the public relay,
including after an automatic service restart. Tailscale remained active with
its existing identity. Actual reboot behavior has not been tested.

Reference: [Upterm v0.24 host options](https://github.com/owenthereal/upterm/blob/v0.24.0/docs/upterm_host.md)
and [published relay key](https://github.com/owenthereal/upterm/blob/master/etc/known_hosts/uptermd.upterm.dev).
