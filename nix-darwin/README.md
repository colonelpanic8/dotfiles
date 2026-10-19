# Kat's MacBook Neo

`kats-macbook-neo` uses a multi-user Determinate Nix installation. Determinate
manages Nix and its daemon; the host module sets `nix.enable = false` so
nix-darwin manages the system packages and shell integration separately.

The shared checkout lives at `/Users/Shared/dotfiles`. To apply changes:

```sh
cd /Users/Shared/dotfiles/nix-darwin
just switch
```

All three Macs use `mkDarwinSystem` for shared CLI tools, desktop applications,
fonts, and Home Manager settings. Host accounts and services live in `machines/`.
The Gitea runner and its storage configuration are exclusive to the Mini.
Kat is the Neo's primary and only managed login account; Alex remains the Air's
primary user. Both laptops preserve unmanaged Homebrew packages.

[`portable-packages.nix`](../nix-shared/portable-packages.nix) is the common utility
list for macOS and NixOS. It includes Git, ripgrep, fd, jq, just, tmux, Zellij,
Node, formatters, and image/media tools. Both systems consume it through
`nix-shared/system/essential.nix`; platform-specific tools stay in that module.

Codex Desktop and Claude Desktop are declared as shared Homebrew casks; their
CLIs come from dedicated Nix flakes. The shared desktop module starts Raycast
automatically, assigns Command-Space, disables Spotlight's conflicting shortcuts,
and adds T3 Code and Ghostty to the Dock. The Mini and Neo run T3 Code system
daemons; the Air retains its desktop client setup.

All Macs use `KeyRepeat = 2`, `InitialKeyRepeat = 15`, and Caps Lock → Control.
The Mini and Neo opt into `persistent-desktop.nix` for their existing sleep and
screen-lock preferences; the Air retains macOS's defaults for those settings.

The Neo has its own encrypted T3 Code credential. Kat's desktop
connects to its local T3 Code daemon; it does not receive Ivan's fleet credential,
GPG secrets, or private repository synchronization configuration.

Tailscale runs as a system daemon and enrolls using
`tailscale-authkey.kats-macbook-neo.age`, encrypted to this Mac's SSH host key
and Kat's Ed25519 key. Kat is the local Tailscale operator. `katKeys` is separate
from the shared SSH authorization and administrator key lists; this host alone
includes it in Kat's authorized keys.

The Neo's reusable enrollment token expires on January 17, 2027. Rotate the
encrypted token before enrolling again after that date. An enrolled daemon
keeps its device state across rebuilds and reboots.

`org.nixos.dotfiles-auto-upgrade` pulls `origin/master` and runs `just switch`
daily at 04:30 local time, or after the Mac wakes. It skips a checkout with local
changes or another branch. Dependency versions follow the committed lockfile.
Logs are in `~/Library/Logs/dotfiles-auto-upgrade{,.err}.log`.

Check the system daemons with:

```sh
sudo launchctl print system/org.nixos.t3code
sudo launchctl print system/org.nixos.dotfiles-auto-upgrade
```

# Tailscale on macOS

The Mini and Neo use the shared
[`tailscale.nix`](tailscale.nix) module. Nix installs the CLI and runs `tailscaled`
as a system launchd daemon, including before a user logs in. A launchd job
reconciles MagicDNS acceptance, the configured hostname, and the primary user's
operator access at startup and every five minutes. Other Tailscale preferences
are left alone.

The Air retains its existing Tailscale.app connection. Its optional daemon
migration is described below; desktop and utility upgrades do not migrate its VPN.

Authentication is separate from these settings:

- The Mini continues to use its existing agenix auth key. A missing, empty, or
  `DISABLED` key skips automatic enrollment.
- The Air's optional daemon setup uses `tailscale-authkey.alexanders-macbook-air.age`, encrypted only to
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
2. Import `../tailscale.nix` in `machines/alexanders-macbook-air.nix`, passing
   `authKeyFile = ../../nixos/secrets/tailscale-authkey.alexanders-macbook-air.age`,
   and set `age.identityPaths = ["/etc/ssh/ssh_host_ed25519_key"]`.
   Activate the configuration:

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
once the new connection works.

For unattended enrollment on another Mac, pass an encrypted file as
`authKeyFile` when importing the module, encrypt it to that Mac's SSH host key,
and configure its agenix identities separately. Never put a plaintext key in the
Nix configuration.
