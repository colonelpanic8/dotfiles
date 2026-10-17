# Tailscale on macOS

Both `mac-demarco-mini` and `alexanders-macbook-air` use the shared
[`tailscale.nix`](tailscale.nix) module. Nix installs the CLI and runs `tailscaled`
as a system launchd daemon, including before a user logs in. A launchd job
reconciles MagicDNS acceptance, the configured hostname, and the primary user's
operator access at startup and every five minutes. Other Tailscale preferences
are left alone.

Authentication is separate from these settings:

- The Mini continues to use its existing agenix auth key. A missing, empty, or
  `DISABLED` key skips automatic enrollment.
- The Air uses `tailscale-authkey.alexanders-macbook-air.age`, encrypted only to
  its SSH host key. Agenix uses `/etc/ssh/ssh_host_ed25519_key` to decrypt it at
  activation, and the root-only plaintext file is passed to Tailscale by path.
  No user SSH identity is needed for unattended enrollment.

The Air's reusable auth key expires after 90 days. Rotate the encrypted secret
before provisioning or reauthenticating after that date. An already enrolled
daemon retains its separate device state across rebuilds and reboots; auth-key
expiration does not expire that device's node key. See
[Tailscale's auth-key documentation](https://tailscale.com/docs/features/access-control/auth-keys).

## Migrate the Air from Tailscale.app

The GUI app and the Nix daemon have separate state. Expect to authenticate the
daemon as a new device; the current app's identity is not imported. This changes
the client to a CLI-only system service. Tailscale's open-source macOS client
also has incomplete Taildrop support and cannot use an exit node.

1. Disconnect and quit Tailscale.app. Remove it from `/Applications`, empty the
   Trash, and reboot, following
   [Tailscale's macOS variant migration guidance](https://tailscale.com/docs/concepts/macos-variants).
   Use a local terminal for this migration because the VPN will disconnect.
2. Activate the configuration:

   ```sh
   cd /Users/Shared/dotfiles/nix-darwin
   just switch
   ```

   Activation refuses to start the daemon while `/Applications/Tailscale.app`
   is installed, to prevent overlapping clients.
3. The launchd job enrolls the daemon automatically using the decrypted key.
   Check its logs if enrollment has not completed:

   ```sh
   sudo tail -n 50 /var/log/tailscale-autoconnect.err.log
   ```

4. Verify connectivity:

   ```sh
   tailscale status
   tailscale ip -4
   sudo launchctl print system/com.tailscale.tailscaled
   ```

The new device may have a different Tailscale IP. Update any references to its
old address and remove the old GUI client's device entry from the admin console
once the new connection works. Paseo discovers the daemon's address directly.

For unattended enrollment on another Mac, pass an encrypted file as
`authKeyFile` when importing the module, encrypt it to that Mac's SSH host key,
and configure its agenix identities separately. Never put a plaintext key in the
Nix configuration.
