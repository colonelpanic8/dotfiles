# NixOS family installer

Prepare a personal installer USB on an existing trusted computer, then install
NixOS through Calamares with T3 Code and the new owner's agent account. The
installer lives in `nixos/family-installer/` in
[dotfiles](https://github.com/colonelpanic8/dotfiles), alongside Ivan's existing
rescue ISO and persistent USB configuration. Alex prepares media from his MacBook
Air's existing `/Users/Shared/dotfiles` checkout. His authenticated Codex account
and a newly generated target host key can be staged before booting the PC. No
separate repository, release upload, or new branch is needed to prepare media.

This is the operating guide for the person installing, a remote helper, and an
assisting agent. Start at the section matching the current state. Commands in
an installed-system section run on the new family computer, not Ivan's workstation.

- [Prepare personal media on Alex's Mac](#prepare-personal-media-on-alexs-mac)
- [Choose the media](#choose-the-media)
- [Build the family ISO](#build-the-family-iso)
- [Prepare the USB on Windows](#prepare-the-usb-on-windows)
- [Write and boot a USB stick](#write-and-boot-a-usb-stick)
- [Connect a remote helper](#connect-a-remote-helper)
- [Install through Calamares](#install-through-calamares)
- [Replace an existing installation](#replace-an-existing-installation)
- [Check the installed computer](#check-the-installed-computer)
- [Register the machine](#register-the-machine)
- [Enable conservative updates](#enable-conservative-updates)
- [Grant persistent support access or secrets](#grant-persistent-support-access-or-secrets)
- [Troubleshoot and recover](#troubleshoot-and-recover)
- [Instructions for an assisting agent](#instructions-for-an-assisting-agent)
- [Maintain the installer](#maintain-the-installer)
- [Implementation reference](#implementation-reference)

## Prepare personal media on Alex's Mac

Use the MacBook Air already on Tailscale, logged in as `alex`. This Mac prepares
the USB; NixOS goes on the separate x86_64 PC. The Mac's internal disk is never
an installation destination. Read [START-HERE.md](START-HERE.md) for the short
live-USB walkthrough and a prompt for the assisting agent.

The base ISO contains T3 Code, Codex, Claude Code, and Calamares. It contains no
provider credentials. Personalization adds a passphrase-encrypted bundle after
the Nix build, outside the Nix store. The bundle contains a new target SSH host
identity and only the explicitly selected accounts/access. Do not publish the
personal ISO or put preparation state into Git.

On the Mac, from the existing checkout:

```sh
cd /Users/Shared/dotfiles/nixos
just family-usb --iso /Users/alex/Downloads/nixos-family-base.iso \
  --authorized-key /Users/alex/.ssh/id_ed25519.pub
```

Use the actual verified base ISO path supplied during preparation. A helper
can build it with `just family-iso` on Linux and copy it directly with SCP;
there is no need to publish it. Compare SHA-256 at both ends before preparing
personal media. Omitting `--iso` builds the base image from this checkout.
That requires x86_64 Linux or a configured Linux builder: Apple Silicon macOS
alone cannot execute x86_64 Linux builds. Nix and an existing clone alone do
not provide a Linux builder.

Defaults are login `alex`, hostname `alex-nixos`, account email
`alexmalison@gmail.com`, and **Another person**. Override with `--username`,
`--hostname`, and `--email` when preparing another person's computer. The
command opens a **separate Codex login for the new PC**, using
`~/.local/state/nixos-family-installer/<hostname>/codex-login` as `CODEX_HOME`.
Sign in as `alexmalison@gmail.com` in the browser when prompted. The command
checks that email before exporting. It never exports the Mac's usual
`~/.codex/auth.json`: sharing a refresh token between independent computers can
invalidate their sessions. After successfully creating the personal ISO, it
removes the temporary login file from the preparation profile. It does not copy
conversations, settings, Git-sync directories, or GPG material. Claude Code is
installed but its account is not exported; sign in separately later if wanted.

The command keeps the prepared host identity under
`~/.local/state/nixos-family-installer/<hostname>/`, reusing it on retries.
Only `identity.json` and the `.pub` file are public. The private host key remains
on this trusted preparation Mac and travels in the encrypted personal bundle.
Inside that bundle, account credentials are additionally encrypted to the host
key. Choose a USB bundle passphrase when prompted; enter it again on the live
USB. This protects the credentials if the USB is lost. It is separate from the
eventual disk-unlock and login passwords. Keep private media out of cloud
sharing and source control.

After preparation, plug in the intended USB and inspect:

```sh
diskutil list external physical
```

Match model and capacity, back up any needed USB files, then write the completed
personal image. Replace `/dev/diskN` with the actual external whole USB disk:

```sh
just family-usb \
  --write-existing /Users/alex/.local/state/nixos-family-installer/alex-nixos/alex-nixos-installer.iso \
  --device /dev/diskN
```

**This erases every file and partition on that USB.** The writer rejects
internal disks, partitions, non-USB devices, read-only media, and undersized
devices. It displays the selected disk and requires typing `ERASE diskN` before
writing. It rechecks identity, writes the image, verifies the bytes, and ejects
the stick. Preparation can also take `--device` to do both stages in one run.
No USB write happens without that explicit device confirmation.

Boot the separate PC from the USB. Connect its network and use **Start personal
setup**. The terminal unlocks the bundle, installs the selected account login
into the live user's home, and launches T3 Code. Open the `Setup` project and
use the prompt in `START-HERE.md`. Choose Codex.
Calamares refuses to partition a personal USB installation until the bundle is
unlocked, and validates the prepared user, hostname, and owner.
Its disk, bootloader, encryption and password screens remain in charge of the
installation. The person must separately confirm the actual internal disk.

At the end of installation, Calamares stops the live Codex processes, encrypts
the live session's current credentials for the target (including token refreshes
during setup), and removes the live login. The live agent disconnecting at this
point is expected. Reboot when Calamares finishes; continue with the agent on
the installed system.
The installed computer receives T3 Code and those credentials at first boot,
then deletes its encrypted account bootstrap file. Later logout or account
changes are preserved. The personal USB is a **single-install credential
transfer**, not reusable authenticated rescue media: booting its original
credential snapshot again may require a fresh login. After a successful install,
erase the personal USB and delete its private ISO and bundle from the Mac when
no longer needed. Keep the public identity record for support.
Credentials remain revocable and may expire or require a later provider login;
preparation is not a promise of permanent authentication. The live T3
conversation is ephemeral, so save needed notes to the target before rebooting.

### Optional pre-authorized remote support

Pass each approved helper's Ed25519 **public** key with `--authorized-key`.
Never pass a private helper key. Prepared targets accept key-based SSH on
`tailscale0`, disable password/root SSH, and do not open SSH to the LAN.

To avoid a Tailscale login at installation, arrange two distinct pre-authorized,
appropriately scoped, single-use enrollment keys ahead of time: one for the live installer,
one for the permanent PC. Pass paths using `--tailscale-live-key` and
`--tailscale-target-key`. The command encrypts each to the new host key; neither
is a Nix input. The live node uses `<hostname>-installer`; the target enrolls
independently at first boot. Its encrypted enrollment key and runtime plaintext
are removed after Tailscale reports Running. If enrollment fails, the encrypted
key remains for retry; replace an expired key or remove that bootstrap file
before retrying with interactive login. Do not copy the Mac's Tailscale state or reuse its
device identity. A tag or auth key alone does not restrict access: verify the
tailnet's policy permits only the intended helper access and does not grant
the family device broad access to Ivan's machines.

Without these keys, Tailscale is available but not pre-authenticated. Codex
assistance and installation still work locally. Neither `--authorized-key`
nor a running Tailscale daemon implies that remote enrollment is complete.
Only report support ready after actually testing the intended connection.

## Choose the media

For Alex's prepared setup, use the **personalized family Plasma ISO on his Mac**.
The current base image contains the custom installer, T3 Code, agent CLIs and Tailscale, and
boots into the desktop used in the installation tests. This is the default
choice for Alex's installation. Use the combined rescue image only when the
helper explicitly chooses it. `rescue-usb` is Ivan's device-specific persistent
installation, not an ISO for another USB stick.

The standalone family ISO boots an ephemeral Plasma desktop. Its default live
account is `nixos`. It includes Calamares and Tailscale. Connect to the internet
before installation because Nix may download dependencies. The desktop session,
its temporary password, and its Tailscale login disappear after reboot.

The same dotfiles checkout contains both rescue configurations:

| Configuration | Purpose | Build from `/srv/dotfiles/nixos` |
| --- | --- | --- |
| `rescue` | Ephemeral Hyprland rescue ISO with agent apps and disk tools | `nix build .#nixosConfigurations.rescue.config.system.build.isoImage` |
| `rescue-usb` | Writable NixOS installation on Ivan's existing SanDisk USB, with persistent state and memory-test boot entries | `nix build .#nixosConfigurations.rescue-usb.config.system.build.toplevel` |

Those configurations use `nixos/rescue-image.nix`. The USB's device-specific
layout is in `nixos/rescue-usb.nix`; it is not a generic image-writing recipe.
Building its `toplevel` does not install or update the physical stick. Never
reuse its hardcoded device path or filesystem UUID for another computer.

From this directory, the reusable module is `modules/calamares.nix`, also exported
as `nixosModules.installer`. A rescue system can import that module to provide
the same graphical installer without changing its desktop, accounts, or boot
layout. The module also installs this guide at
`/etc/nixos-family-installer/README.md` and the agent instructions alongside it.
An older rescue stick needs an update before it has this integration.

The rescue account is `imalison`, with passwordless local sudo. A persistent
rescue USB can retain app logins, SSH keys, Tailscale state, and other files.
Use a fresh family ISO when handing media to someone who must not receive that
state. The installer generates the target configuration from the choices in
Calamares; it does not clone the rescue system's accounts or home directory.

## Build the family ISO

Build the current checkout on x86_64 Linux with Git and Nix. Flakes must be available.
For Alex's personal media, use the current `master` checkout containing this
workflow. The older published generic ISO does not contain personal enrollment.
`family-stable` controls reviewed updates to installed systems; preparing this
USB does not require switching the Mac to that branch.
The project provides its remaining development tools through Nix.

```sh
git clone https://github.com/colonelpanic8/dotfiles.git
cd dotfiles
cd nixos/family-installer
nix develop --command just check
nix flake check
nix develop --command just build
```

With direnv, run `direnv allow` after reviewing `.envrc`, then use
`direnv exec . just check` and `direnv exec . just build` instead.

The ISO appears under `result/iso/`. Record its checksum and the release commit
so a helper can identify the exact media:

```sh
git rev-parse HEAD
sha256sum result/iso/*.iso
```

`master` is the dotfiles development branch. `family-stable` is the manually
promoted family release in the same repository. Workstation commits do not
advance `family-stable` or update existing family computers. This directory has
its own Nixpkgs and Home Manager lockfile. Building it does not evaluate the
workstation flake or fetch its private inputs, and needs no Git submodules.

From `/srv/dotfiles/nixos`, `just family-iso` builds the standalone image and
`just rescue-iso` builds the combined rescue image. `just rescue-usb-build`
builds the persistent USB system without writing a disk or activating it.
These recipes create `nixos/result-family`, `nixos/result-rescue`, and
`nixos/result-rescue-usb`, respectively. Only the first two contain ISO files.

To exercise the ISO without a physical disk, run:

```sh
nix develop --command just vm
```

That recipe creates a disposable 40 GiB virtual disk and prints its path. It
uses KVM when available. Keep the disk to inspect the installation, or remove
that printed file after shutting down the VM. The recipe does not test UEFI;
use an OVMF-configured VM for a UEFI installation test.

You can also build the family image from its public release URL on x86_64
Linux with Nix and Git, without a local dotfiles checkout:

```sh
nix build 'git+https://github.com/colonelpanic8/dotfiles?dir=nixos/family-installer&ref=family-stable#iso' --out-link result-family
sha256sum result-family/iso/*.iso
```

If Nix has not enabled flakes, add
`--extra-experimental-features 'nix-command flakes'` to the `nix` command.
Updating a checkout supplies the source and recipes; the build command still
has to run to produce an ISO. Windows alone cannot build this Linux image.
A configured x86_64 Linux environment, including WSL2 with Nix, can build it.
A Windows computer only needs the completed ISO to write the USB.

## Prepare the USB on Windows

The older prebuilt [family installer release](https://github.com/colonelpanic8/dotfiles/releases/tag/family-installer-2026-10-21)
is a fallback generic installer. It predates personal agent enrollment; use the
prepared Mac image for Alex's current setup. To use that older release, download `Download-Installer.ps1` from
that release. Ask the assisting agent to inspect it, then run it in PowerShell:

```powershell
powershell.exe -NoProfile -ExecutionPolicy Bypass -File .\Download-Installer.ps1
```

This sets the execution policy only for that process. The script downloads two
image parts, verifies their SHA-256 checksums, joins them, and verifies the full
ISO. It prints the verified file's path under your Downloads folder. It writes
no disks. The parts are needed because GitHub limits each release asset to
2 GiB. Do not flash a `.part01` or `.part02` file, the script, or a source ZIP.
Use the assembled `.iso` file. `SHA256SUMS` and `BUILD-INFO.txt` on the release
record the checksum and exact dotfiles source commit.

If the helper supplies a complete ISO instead, use that file and its checksum.
The helper can build both from the release command above. Ask for the actual
image file or a working download link; a path on the helper's computer is not
a download. A source ZIP from GitHub is not an ISO. Confirm the destination
computer supports x86_64; the preparation computer's architecture does not
establish the destination's architecture.

**Writing the ISO erases every file and partition on the selected USB.**
Back up its files. Unplug other removable drives to reduce selection mistakes.

1. Download the ISO to the Windows computer. If a shared Drive file requires
   login, use the email address to which the helper granted access.
2. Open PowerShell. Replace the path with the actual downloaded file:

   ```powershell
   Get-FileHash -Algorithm SHA256 -LiteralPath 'C:\Users\YOUR-USER\Downloads\YOUR-IMAGE.iso'
   ```

   Compare all 64 hexadecimal digits with the helper's checksum. Letter case
   does not matter. If they differ, download again and do not flash that file.
3. Download Windows x64 [balenaEtcher](https://etcher.balena.io/) from its
   official site. Use **Flash from file** and select the verified ISO.
4. Use **Select target**. Match the USB's model and capacity to the device
   the person has confirmed may be erased. Do not select an internal drive
   or a duplicate USB entry that reports no media. Do not rely on a remembered
   Windows disk number after unplugging or restarting.
5. Review the selected USB and click **Flash**. Accept the administrator
   prompt for the writer. Let both writing and validation finish successfully.
6. If Windows offers to format the newly written USB, cancel. The installer
   has Linux partitions Windows may not recognize. Eject the stick if it is
   still mounted. Do not manually partition or preformat it.
7. Move the USB to the destination computer and follow the boot instructions
   below. Writing media does not install NixOS on the Windows preparation PC.

Etcher's [official guide and downloads](https://etcher.balena.io/) describe
image selection, target selection, flashing, and validation. Creating media
needs neither a Nix installation nor WSL on the Windows computer.

## Write and boot a USB stick

Back up files on the destination computer before installing an operating
system. Also back up the USB stick; writing an ISO replaces its contents.
Do not overwrite Ivan's persistent rescue USB unless he intends to replace it.

Use a disk-image writer's raw image mode, not a normal file copy. On Linux,
inspect the available devices before selecting the USB:

```sh
lsblk -p -o NAME,SIZE,MODEL,TRAN,FSTYPE,MOUNTPOINTS
ls -l /dev/disk/by-id/usb-*
```

Match the USB by model and capacity. Unmount its mounted partitions using the
file manager. The output device must be the whole USB device, not a partition
such as `/dev/sdb1` or a by-id name ending in `-part1`.

For a command-line write, replace both placeholders with the paths you have
checked. These commands are examples; an agent must have confirmation of the
actual destination device before executing the write.

```sh
ISO=/absolute/path/to/nixos-family-image.iso
USB_DEVICE=/dev/disk/by-id/usb-REPLACE_WITH_CONFIRMED_DEVICE
test -f "$ISO" && test -b "$USB_DEVICE"
lsblk -p -o NAME,SIZE,MODEL,TRAN,MOUNTPOINTS "$USB_DEVICE"
sudo dd if="$ISO" of="$USB_DEVICE" bs=4M conv=fsync status=progress
sync
```

If the file or device check fails, stop and correct the paths before running
`dd`. Do not substitute a guessed `/dev/sdX` name.

Restart the destination computer and select the USB in its firmware boot menu.
Use its UEFI entry for a UEFI installation. If Secure Boot rejects the image,
record the existing firmware setting before disabling Secure Boot for this
unsigned media. Confirm any disk-recovery requirements of the existing OS
before changing its firmware settings.

A family ISO reaches a Plasma desktop. Open **Install System** if Calamares
does not open automatically. A combined rescue build reaches Hyprland; open
its application launcher with Super+P and choose **Install System**, or run
`family-installer` from Ghostty with Super+Shift+Return.

## Connect a remote helper

Someone at the computer must connect Wi-Fi or Ethernet and complete any login
shown by Tailscale. On the family ISO, open Konsole and run:

```sh
sudo tailscale up
sudo passwd nixos
tailscale ip -4
```

Set a temporary password for the live `nixos` account. Share it privately with
the helper, along with the printed Tailscale address. Do not put passwords or
authentication links in a public issue, PR, or transcript.

On a combined rescue system, the live user is `imalison`. Use
`sudo passwd imalison` if password-based SSH support is needed. On an existing
persistent rescue USB, first inspect `tailscale status`; avoid replacing its
existing tailnet identity or resetting its account password without Ivan's
agreement. The rescue system may already authorize Ivan's SSH public keys.

Ivan approves the device and limits its access in the tailnet's policy. Merely
joining a tailnet does not prove that the intended access restrictions exist.
No reusable Tailscale enrollment key is embedded in the family ISO.

From an authorized helper machine, connect with the actual live username:

```sh
ssh nixos@100.x.y.z
```

Check the live host's fingerprint through the person at the computer before
accepting a new SSH identity. The live host and the installed computer have
separate identities. A changed fingerprint after reboot requires verification.

SSH provides a shell. Graphical screen sharing is not configured. The person
at the computer clicks through Calamares while the helper explains choices
and reads relevant diagnostics. An agent on generic media needs its own login.
Personal media explicitly supplies the accounts selected during preparation.

Keep a way to contact the person outside this SSH session. Installation and
reboot end the live session. Generic installations do not inherit the live
Tailscale or agent login. Personal media provisions the target separately;
temporary live passwords and live Tailscale state are still not copied.

## Install through Calamares

1. Choose the language, location, and keyboard layout. Confirm the keyboard
   before entering passwords, particularly when it differs from US English.
2. On **Primary user**, select **Another person** for Dad. Select **Ivan** only
   when Ivan's Git name and email are wanted. This choice grants no secrets.
3. On **Users**, enter the person's full name, a lowercase login name, and a
   unique computer name. For example, use login `dad` and computer `dad-laptop`.
   The login name can differ from `imalison` even when choosing Ivan.
4. Enter the account password and administrator password. Calamares offers a
   checkbox to use the same password for both. Keep these passwords private.
5. Select a desktop. Plasma is the desktop tested end to end with this installer.
6. Decide whether the hardware needs unfree software. Some graphics and network
   hardware require it. The choice affects the generated target configuration.
7. On **Partitions**, identify the destination by model and capacity. Exclude
   the boot USB and every disk containing data that must be preserved.
8. Use **Erase disk** only when replacing all contents of that destination is
   intended. Existing dual-boot layouts and manual partitioning need individual
   review. This project has not verified every layout offered by Calamares.
9. If selecting **Encrypt system**, record the disk-unlock passphrase privately.
   It is separate from the account password. The tested UEFI layout uses a
   FAT32 boot partition and a LUKS2-encrypted root filesystem.
10. Review **Summary** with the person at the computer. Confirm the destination,
    deletion, partition layout, and encryption before clicking **Install**.
    The stock configuration does not show another confirmation dialog.
11. Wait for **All done**. If installation fails, collect diagnostics before
    rebooting. An error after partitioning may leave the disk partially installed.
12. Select **Restart now**, finish, and boot the internal disk. Remove the USB
    when it is safe to do so, or select the internal disk in the firmware menu.

A computer name must be at most 63 characters, start and end with a letter or
digit, and contain only letters, digits, underscores, or hyphens. Avoid
underscores for compatibility with other network tools. `localhost` and
`installer` are reserved. Login names are lowercase, at most 31 characters,
and cannot be `root` or `nobody`.

At boot, enter the disk passphrase if encryption is enabled. Then sign in with
the account password. SSH cannot unlock the encrypted disk before boot; this
project does not configure remote unlocking in the initrd.

## Replace an existing installation

For a computer whose existing OS and files can all be discarded, use
Calamares' **Erase disk** option on the confirmed internal destination disk.
This replaces the entire installation. It does not preserve the previous OS,
recovery partitions, applications, or personal files on that disk.

1. Before booting the installer, finish backups to another disk or a remote
   location. Disconnect any backup disk before partitioning.
2. Boot the USB in UEFI mode if the destination computer supports UEFI. Use
   **Another person**, the intended primary login, a unique hostname, and
   **Plasma**. Connect the helper before starting the installation.
3. On **Partitions**, select the internal destination by model and capacity.
   The installer USB is a separate device and must remain excluded. If there
   are multiple internal disks, identify the one whose contents may be removed.
4. Have the person confirm that this exact disk may be erased in full and
   that needed files are backed up. Then choose **Erase disk**. Do not choose
   install alongside, replace one partition, or manual partitioning for this
   whole-disk replacement. Let Calamares generate the partition layout.
5. If offered a filesystem choice, ext4 is suitable for this setup. Under UEFI,
   the generated plan must include an EFI System Partition mounted at
   `/boot` and a root filesystem mounted at `/`. An encrypted installation
   places the root filesystem inside LUKS; the EFI partition stays unencrypted.
   Use the installer's automatic sizes rather than entering device names or
   a partition table copied from another computer. Swap choices can use the
   installer defaults; hibernation is not required for this setup.
6. Decide on **Encrypt system** with the person. If enabled, they must retain
   the separate disk passphrase and enter it locally after every reboot.
   Remote SSH cannot unlock it. Do not choose or disclose the passphrase for them.
7. Review **Summary** together. Check the internal disk model and capacity,
   the full-disk deletion, the boot mode, and the proposed EFI and root mounts.
   If the USB, another disk, or an unexpected layout is listed for deletion,
   stop and correct the selection. **Install** starts disk writes without
   another confirmation dialog.
8. After installation completes, restart from the internal disk, remove the
   USB when safe, unlock the disk if encrypted, and sign in to the new account.
   Verify this boot before registering the machine or enabling managed updates.

The plan to replace the old installation does not identify a device by itself.
An assisting agent still needs the person's confirmation of the actual disk.
Booting the installer or confirming the USB is not confirmation to erase the
internal disk. On legacy BIOS hardware, the automatic bootloader layout is
different; have the helper review it instead of forcing the UEFI layout above.

## Check the installed computer

Run these commands in a terminal on the newly installed computer:

```sh
whoami
hostname
findmnt -n -o SOURCE,FSTYPE /
readlink -f /etc/nixos
nix eval --expr '1 + 1'
command -v zsh tmux zellij git
```

Expect the chosen login and computer name, the internal root filesystem,
`/srv/nixos-family-installer` as the configuration path, and `2` from Nix.
An encrypted root normally appears under `/dev/mapper/`.

Both owner choices receive flakes, Zsh, tmux, Zellij, shared multiplexer aliases,
and Git. Ivan receives his Git name and email. Another person receives no
inherited Git identity. Check the latter with `git config --get user.email`;
an unset value produces no output and a nonzero exit status.

The installer imports no personal GPG keys, password store, Git-sync repositories,
or Syncthing folders. Agent credentials are included only through explicit
personal media preparation. It does not create Ivan's account
in addition to the chosen primary account. The profile is a shared subset of
dotfiles, not the full workstation configuration or a symlink to Ivan's home.

Inspect the public enrollment request and host-key fingerprint:

```sh
cat /var/lib/nixos-family-installer/enrollment.json
ssh-keygen -lf /etc/ssh/ssh_host_ed25519_key.pub
stat -c '%a %U %n' /etc/ssh/ssh_host_ed25519_key
```

The request names the chosen machine and user, includes its public host key,
and requests no secrets. The private host-key file belongs to root with mode
`600`. Never print the private key or transfer it outside the documented trusted
Mac → encrypted personal media → target path.

Automatic updates are initially disabled. An absent `nixos-upgrade.timer` is
expected until registration. Installation can succeed without tailnet access,
secret approval, or an upstream PR.

## Register the machine

Do this from Ivan's trusted workstation after verifying the new computer's
public host-key fingerprint through the support session. Obtain these files
through an authorized connection or a reviewed removable-media transfer:

- `/srv/nixos-family-installer/machines/<hostname>/`, including `default.nix`,
  `identity.json`, `configuration.nix`, and `hardware-configuration.nix`.
- `/var/lib/nixos-family-installer/enrollment.json`.

The initial remote live session ends at reboot. If persistent SSH was not
configured, ask the person at the machine to assist with the transfer. Do not
assume that the pre-installation Tailscale address still reaches the computer.

Copy the machine directory into `nixos/family-installer/machines/<hostname>/`
in the primary dotfiles checkout. Keep it as an independent machine definition;
do not import the rescue hardware configuration or Ivan's workstation module.
Inspect every file for accidental private data before committing or opening a
public PR. Generated filesystem UUIDs and hardware details also become public
when committed here. The public enrollment request is for review; it is not a
credential and does not authorize any grant.

On the workstation, replace `dad-laptop` below with the real hostname:

```sh
cd /srv/dotfiles
git switch -c register-dad-laptop
git add nixos/family-installer/machines/dad-laptop
cd nixos/family-installer
nix develop --command just check
nix build .#nixosConfigurations.dad-laptop.config.system.build.toplevel
```

A Git-backed Nix flake sees new files only after they are added to Git. Review
the configuration and build result before publishing the branch or creating
a PR. GitHub authentication stays on the trusted workstation. The installer
does not open PRs or re-encrypt secrets automatically.

## Enable conservative updates

Register and verify the machine with updates disabled first. Once its definition
exists on the published `family-stable` branch, add this option to that machine's
module and build the change:

```nix
familyInstaller.updates.enable = true;
```

Review and publish that change through `family-stable` before activating it on
the family computer. On that computer, use its real hostname in the quoted URL:

```sh
sudo nixos-rebuild switch --flake 'git+https://github.com/colonelpanic8/dotfiles?dir=nixos/family-installer&ref=family-stable#dad-laptop'
systemctl status nixos-upgrade.timer
systemctl list-timers nixos-upgrade.timer
```

This activation command is for the family computer. Ivan's existing dotfiles
workstation and writable rescue system have their own rebuild instructions;
use `just switch` from their primary `/srv/dotfiles/nixos` checkout.

Updates run daily with up to 45 minutes of randomized delay. They use the
release's lockfile, retain boot entries, and do not reboot automatically. An
update may still restart services. `family-stable` is a manually maintained
branch, not an automatic test or approval gate. Its maintainers must build
and verify changes before promoting it.

Keep local edits in `/etc/nixos` under review. A successful remote update does
not rewrite that editable directory, so it may describe an older configuration
than `/run/current-system`. Use the published release URL for managed rebuilds
and carry desired local changes back into the registered machine definition.
Do not change `system.stateVersion` merely to match a newer NixOS release.

To stop managed updates on a registered computer, set
`familyInstaller.updates.enable = false;` in its machine module, build, and
activate that configuration. For an immediate temporary pause on the family
computer, stop the timer with `sudo systemctl stop nixos-upgrade.timer`; a
reboot or later activation can start it again.

## Grant persistent support access or secrets

Persistent support is an explicit change to the installed machine. For a
reviewed Tailscale and SSH setup, its machine module can contain:

```nix
services.tailscale.enable = true;
services.openssh.enable = true;
users.users.dad.openssh.authorizedKeys.keys = [
	"ssh-ed25519 REPLACE_WITH_APPROVED_HELPER_PUBLIC_KEY"
];
```

Replace the account name and key before building. Decide which network
interfaces may reach SSH and apply the intended Tailscale policy. This example
enables an SSH server; it is not a complete access policy. After activating the
configuration, enroll with `sudo tailscale up` on the installed computer and
verify that only the intended helper can connect.

The generated Ed25519 identity is an SSH **host** key, not the primary user's
SSH key and not Ivan's GPG key. Retry during preparation preserves it. An
approved agenix recipient can use this public key, but the safe shared profile
does not import agenix or any encrypted secret by default.

For each proposed secret, review its purpose and grant only that secret to
this host. Do not add a family machine to dotfiles' broad `hostKeys`,
`sshClientKeys`, or `agenixKeys` groups. Never copy Ivan's private GPG key,
password store, agent credentials, or Git-sync directories to satisfy an
installation or authentication failure. Choosing **Ivan** does not grant them.

## Troubleshoot and recover

### The USB does not boot

Check that the writer used raw image mode, selected the intended whole device,
and finished syncing. Compare the original ISO checksum with the recorded
build checksum. Inspect the firmware's USB and UEFI choices. If possible, try
the ISO in a VM before rewriting a stick or changing more firmware settings.

### Wi-Fi or remote access fails

Use the desktop network applet or `nmtui`. A wired connection can avoid an
unsupported Wi-Fi device during installation. Inspect `ip address`,
`tailscale status`, and `systemctl status tailscaled sshd`. Check the actual
live account, network access, device approval, and tailnet policy. Do not
resolve a connection failure by granting the machine broad secret access.

### Calamares reports an error

Launch through **Install System** or `family-installer`. The launcher uses the
privileged wrapper and permits the local root user to display the installer
on Xwayland. Running the bare `calamares` binary as an ordinary user cannot
complete privileged disk operations. A `pkexec must be setuid root` error on
older combined media requires rebuilding with the current installer module.

Keep the live environment running. Use **Toggle log** and inspect the session
log. In the tested family ISO, the privileged session log was at
`/root/.cache/calamares/session.log`; another launch context may place it under
`~/.cache/calamares/session.log` instead.

```sh
sudo tail -n 100 /root/.cache/calamares/session.log
lsblk -f
findmnt
```

Record the failed job, exact error, ISO commit, selected disk, firmware mode,
and whether partitioning already ran. Review and redact logs before sharing;
logs can contain usernames, hardware details, and other sensitive information.
Do not reboot until the needed diagnostics have been saved outside the
ephemeral live filesystem.

The helper rejects invalid identities before partitioning. If validation fails,
correct the user or computer name. If preparation says an existing checkout
cannot be overwritten, inspect it before making any change. Never delete a
configuration directory just to bypass that protection.

If an old image reports an unsupported `submodules` parameter on a **path**
URL, rebuild from the current family release. The consolidated installer has
no submodule dependency or `self.submodules` hint. Old media retain their
original source and documentation until rebuilt.

### Installation finished but boot or login fails

Confirm the firmware is booting the internal disk. Distinguish the disk-unlock
passphrase from the account password and confirm the keyboard layout. A remote
helper cannot reach the normal SSH server while the computer waits for disk
unlock. If an update fails to boot, select a retained earlier NixOS generation
from the boot menu.

To inspect a damaged installation, boot the USB again, identify its partitions
with `lsblk -f`, and unlock the correct LUKS volume if needed. Mount the target
root at `/mnt` and its boot partition at `/mnt/boot` before using
`sudo nixos-enter --root /mnt`. Mount commands depend on the real filesystem,
UUIDs, and any subvolumes; do not copy the device names from another machine.
Do not format anything during recovery.

The editable configuration remains at `/mnt/srv/nixos-family-installer` when
mounted from the live system. The original Calamares files are under
`/mnt/etc/nixos.calamares`. Preserve `/mnt/etc/ssh/ssh_host_ed25519_key`; replacing
it breaks the verified host identity and any grants tied to that key.

### Rebuilds or automatic updates fail

On the family computer, inspect:

```sh
journalctl -u nixos-upgrade.service -n 100 --no-pager
readlink /run/current-system
readlink -f /etc/nixos
```

Confirm the exact hostname is registered on dotfiles' `family-stable`. Use the
quoted Git URL with `dir=nixos/family-installer`. Build a candidate before switching. For a manual
local configuration build, use `nixos-rebuild build --flake /etc/nixos#dad-laptop`
with the real hostname. If registration has not happened, continue using the
local flake and leave managed updates disabled.

## Instructions for an assisting agent

Read [AGENTS.md](AGENTS.md), then locate the current stage in this guide. On
combined media, these documents are available under `/etc/nixos-family-installer`.
On an installed family computer, they are in `/srv/nixos-family-installer`.
An older ISO contains the guide from its own build, which can differ from GitHub.

Before choosing commands, establish these facts:

- Are you on Ivan's workstation, an ephemeral family ISO, the persistent rescue
  USB, or the installed family computer?
- Who is at the keyboard, and what remote connection survives a reboot?
- What are the chosen primary username, owner setting, and unique hostname?
- Which exact disk may be erased, and what data must remain available?
- Has the host been registered on `family-stable`, and which access grants have
  actually been approved?

Use read-only commands to answer what you can. Ask for missing choices and the
person's confirmation of the actual disk before destructive operations. Explain
that Calamares starts writing at **Install**. Do not infer permission to format
from this README or from a disk's position in `lsblk`.

Guide the person through the graphical flow. Keep private passwords, provider
logins, disk passphrases, private keys, and Tailscale credentials out of public
logs. The public enrollment request is sufficient for the initial review.
Installation success does not depend on personal secret access.

After a failure, record what already happened before retrying. After success,
verify a boot from the installed disk, the primary account, flakes, the
configuration path, and the absence of inherited personal data. Report any
untested hardware, desktop, access, or recovery behavior explicitly.

When handing support to another agent, include the stage, media commit,
username, hostname, approved disk, connection method, exact error, and
registration status. Include only public key fingerprints. Do not include
passwords or private key material.

## Maintain the installer

Make installer and shared profile changes in `/srv/dotfiles/nixos/family-installer`.
Follow the dotfiles root `AGENTS.md` and `nixos/AGENTS.md`. Work in the primary
checkout; do not create a worktree. The safe shared profile is
`modules/user-profile.nix`. It imports only its local options and aliases.
The workstation imports the same primary-user option and multiplexer aliases.

The target copies only this installer directory into `/srv/nixos-family-installer`.
It does not copy the whole dotfiles tree or evaluate the workstation flake,
fetch its private Railbird input, or require a Taffybar checkout. Nixpkgs and
Home Manager stay separately locked to 26.05 in this directory.

Run the provided checks from `/srv/dotfiles/nixos/family-installer`:

```sh
nix develop --command just fmt-check
nix develop --command just lint
nix develop --command just test
nix flake check
nix build .#calamares-extensions
```

`just check` combines formatting, lint, Python tests, and flake evaluation.
`nix flake check` also builds the checks. An ISO build alone does not prove
that installation succeeds. For behavior changes, test the graphical choices,
partitioning and encryption, completion, reboot, login, target configuration,
and personal-data exclusions in a disposable VM before promoting the release.
For rescue integration changes, also check its account, desktop, and boot
configuration; the installer module must not replace those settings.

Promote a reviewed dotfiles commit by fast-forwarding its `family-stable`
branch and pushing it. Do not force-push the release branch or advance it
merely to match the latest workstation change. The release URL selects
`nixos/family-installer` at that commit and uses its own lockfile. A new image build is required
for USB media to include changed code or documentation. Updating GitHub alone
does not update an already-written USB.

## Implementation reference

| File or output | Responsibility |
| --- | --- |
| `modules/iso.nix` | Standalone Plasma media and live Tailscale daemon |
| `modules/calamares.nix` | Reusable installer packages, customization, and installed guide |
| `calamares/owner.conf` | Ivan or Another person selection |
| `calamares/family-validate/` | Identity validation before disk operations |
| `scripts/patch_calamares.py` | Checked integration points in the pinned upstream installer |
| `scripts/prepare_target.py` | Target source, machine definition, host identity, and final configuration link |
| `modules/managed-system.nix` | Registered-machine update options and retention defaults |
| `machines/<hostname>/` | Reviewed machine registrations |
| `modules/user-profile.nix` | Safe shared primary-user profile |
| `packages.x86_64-linux.iso` | Standalone bootable family ISO |
| `nixosModules.installer` | Installer module for another NixOS rescue environment |

The installed filesystem contains:

| Path | Contents |
| --- | --- |
| `/srv/nixos-family-installer` | Release source plus the generated machine directory, with no Git metadata |
| `/etc/nixos` | Relative link to that local flake |
| `/etc/nixos.calamares` | Original generated configuration and hardware files |
| `/etc/ssh/ssh_host_ed25519_key` | Permanent private host key |
| `/etc/ssh/ssh_host_ed25519_key.pub` | Matching public key |
| `/var/lib/nixos-family-installer/enrollment.json` | Public identity, key, empty secret request, and pending-registration status |

The copied source is editable by root, not a Git checkout. It is a
self-contained flake with no parent-directory imports or submodule dependency.
The managed update URL uses `dir=nixos/family-installer` to select the same
flake within the published dotfiles repository.

The installer sets the target's initial `system.stateVersion` from its own
pinned NixOS release, even when the rescue environment runs a newer release.
The installer preserves Calamares' generated disks, bootloader, desktop,
locale, and account settings. Calamares sets the chosen passwords separately.
Preparing the flake preserves an existing generated host key when retried.
Finishing moves the original configuration aside and links `/etc/nixos` to the
local flake. Unrelated existing checkouts and backups cause an error rather
than being overwritten.

The standalone installer passed formatting, lint, behavior tests, and Nix
checks. An x86_64 VM completed the Plasma flow with a non-Ivan account, UEFI,
and LUKS2 encryption. It rebooted from the installed disk and accepted the
chosen disk and account passwords. Checks in that system verified the local
flake, shared tools, host identity, empty secret request, disabled initial
updates, and personal-data exclusions. The combined rescue image also completed an
encrypted UEFI installation and target login in a VM; a fresh image boot
verified Hyprland and the custom installer launcher. Physical hardware, other
desktops, and remote support access require their own verification. After this
source relocation, repeat the preparation and configuration checks before
building new media; prior VM results describe the installer before the move.

The installer's original code is MIT licensed. Bundled NixOS, Calamares, and
other dependencies retain their upstream licenses.
