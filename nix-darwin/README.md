# Kat's MacBook Neo

`kats-macbook-neo` uses a multi-user Determinate Nix installation. Determinate
manages Nix and its daemon; the host module sets `nix.enable = false` so
nix-darwin manages the system packages and shell integration separately.

The shared checkout lives at `/Users/Shared/dotfiles`. To apply changes:

```sh
cd /Users/Shared/dotfiles/nix-darwin
just switch
```

If macOS asks for the **Nix Store** password after restarting even though
Determinate mounts `/nix` automatically, repair the system Keychain lookup:

```sh
sudo /usr/bin/python3 /Users/Shared/dotfiles/nix-darwin/repair-nix-store-keychain.py
```

The helper verifies the installer's existing password without unmounting the
store, then saves a copy under the volume UUID that macOS's APFS unlock agent
searches for. It retains encryption and the installer's original Keychain entry;
passwords are passed through memory and stdin, never command arguments or files.
This is a one-time repair per volume, and is safe to rerun. It requires Apple's
Command Line Tools for `/usr/bin/python3`.

Every Mac's activation creates a `dotfiles` group containing its primary user
and personal Home Manager users. It repairs `/Users/Shared/dotfiles` to use that
group, keeps existing file owners, and enables inherited group write access for
new files and directories, including Git metadata. Symlink targets are left
alone. Git group sharing and each user's trust for this checkout are configured
automatically. If the checkout does not exist yet, clone it there and run
`just switch` again. Restart existing login sessions to refresh group membership.

To repair permissions before the first rebuild:

```sh
sudo /bin/bash /Users/Shared/dotfiles/nix-darwin/setup-shared-checkout.sh /Users/Shared/dotfiles "$USER"
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
daemons; the Air also runs a T3 Code daemon with its own encrypted credential.

All Macs use `KeyRepeat = 2`, `InitialKeyRepeat = 15`, and Caps Lock → Control.
The shared Hammerspoon configuration maps Right Command to Hyper (⌃⌥⌘), with
Shift available as a separate layer. Left Command and Caps Lock → Control keep
their normal behavior. Rebuilds reload Hammerspoon for running managed users;
new login sessions load the same configuration automatically. Hammerspoon needs
macOS Accessibility permission. Hyper remapping pauses during Secure Input.
The Mini and Neo opt into `persistent-desktop.nix` for their existing sleep and
screen-lock preferences; the Air retains macOS's defaults for those settings.
All three also use [`keep-awake.nix`](keep-awake.nix): a system LaunchDaemon runs
macOS's built-in `caffeinate -is` to prevent idle system sleep on AC and battery
power, keeping unattended feeds reachable. Displays can still sleep and lock.
Set `services.keep-awake.onBattery = false` to limit keep-awake to AC power.
Manual sleep and the laptop lid's hardware restrictions still apply.

The Neo has its own encrypted T3 Code credential. Kat's desktop
connects to its local T3 Code daemon; it does not receive Ivan's fleet credential,
GPG secrets, or private repository synchronization configuration.

Ivan's family connections are declared in `nix-shared/t3code-family.nix`.
Home Manager installs persistent SSH tunnels with pinned host keys and a
managed T3 connection registry. Each connection reads its own agenix credential;
no pairing step is required. Group membership and hidden defaults derive from
the same declaration. Session visibility switches do not change these files.
The Air's server listens on loopback and is reached through the SSH tunnel.

## Private camera and microphone feeds

All Macs import [`camera.nix`](camera.nix). OBS captures the primary user's
camera and default microphone after GUI login. Its dedicated `TailnetCamera`
profile uses hardware H.264 encoding at 720p/15fps and publishes only to a
loopback MediaMTX relay. It does not capture the display or desktop audio.
No recordings are enabled; HLS segments remain in a short memory buffer.

Open `https://<machine's-Tailscale-DNS-name>:8443/camera/` from a device signed
into **ivanmalison@gmail.com**. The player starts muted; use its audio control
to listen. Tailscale Serve supplies the authenticated login and the gateway
rejects every other login, including missing or duplicate identity headers.
The relay and gateway listen only on `127.0.0.1` (ports 18379–18381). Port 443
and its existing T3 Code route are separate. This configuration never enables
Funnel. Local users and administrators remain trusted on their own Mac.
Devices with Tailscale tags have no user identity and are denied as well.

Tailscale Serve/HTTPS must be enabled in the Mac's owning tailnet. When it is
disabled, `~/Library/Logs/mac-camera-serve.err.log` contains the approval link;
open it with an administrator of that tailnet. Shared-device access from a
different tailnet does not grant permission to enable Serve. The job retries
every five minutes without changing existing routes on other ports.

OBS needs its normal macOS **Camera** and **Microphone** permission grants once
per login account. In its initial permission window, grant those two and click
Continue; Screen Recording, Accessibility, and Input Monitoring are unnecessary.
Permissions cannot be supplied by Nix. OBS remains visible in the Dock and
macOS displays its camera/microphone activity indicators.

The shared Hammerspoon **Camera/Mic** menu offers Pause, Resume, and OBS controls.
The primary user can also run:

```sh
mac-camera pause
mac-camera resume
mac-camera status
mac-camera status --json
```

For unattended SSH control, use `mac-camera start` and `mac-camera stop` (aliases
for Resume and Pause). For example:

```sh
ssh kat@kats-macbook-neo /run/current-system/sw/bin/mac-camera start
ssh kat@kats-macbook-neo /run/current-system/sw/bin/mac-camera status --json
ssh kat@kats-macbook-neo /run/current-system/sw/bin/mac-camera stop
```

Startup launches OBS hidden and minimized to its menu-bar icon, without taking
keyboard focus or opening its main window. Its local controls and macOS capture
indicators remain available. Start leaves an active stream running and restarts
the dedicated profile if its OBS window was left open with streaming stopped.
An OBS session using another collection is left alone. No screen-unlock or
keyboard/mouse automation is used by these commands.

Pause stops the dedicated OBS process immediately and persists across rebuilds,
logins, and reboots. Resume starts it through the managed launch agent. Other OBS
profiles are preserved; an already-running OBS session is left alone at startup.
Stop Streaming in OBS stops publication; Pause also releases its capture devices.
The launch agent restarts failed capture processes; a normal Quit stays stopped
until Resume or the next login.

Capture requires the primary user's GUI session and an awake Mac. It does not
run before login or while asleep. A closed Apple Silicon laptop lid disconnects
the built-in microphone and obstructs its camera. Macs without a camera or input
device skip OBS startup; attach hardware and run Resume. The Mini currently has
neither connected. Alex's Air retains its existing Tailscale.app and DNS name.

See [Tailscale Serve identity headers](https://tailscale.com/docs/features/tailscale-serve#identity-headers)
and [Apple's capture permission documentation](https://developer.apple.com/documentation/bundleresources/requesting-authorization-for-media-capture-on-macos).

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
