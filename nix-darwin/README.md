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
- The Air uses a one-time interactive login. No Tailscale auth secret or
  decryption identity is added for it. The daemon retains its login state on
  disk across rebuilds and reboots; reauthentication remains manual when needed.

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
3. Enroll the daemon and follow the printed browser login URL:

   ```sh
   sudo /run/current-system/sw/bin/tailscale up \
     --accept-dns=true --hostname=alexanders-macbook-air --operator=alex
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
`authKeyFile` when importing the module and configure that host's agenix
identities separately. Never put a plaintext key in the Nix configuration.
