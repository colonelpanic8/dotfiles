# Set up this computer

After booting the personal USB, connect Wi-Fi or Ethernet. **Start personal
setup** asks for the USB bundle passphrase chosen on the Mac, then opens T3
Code. Use **Codex**; it has the account prepared on the Mac. Claude requires a
separate login if wanted later. Select the `Setup` project (or open `~/Setup`).

Paste this into a new Codex thread:

> Help me install NixOS on this computer. Read README.md and AGENTS.md here
> first. We are on the live installer USB, not the Mac that prepared it. Read
> /run/family-installer.ready to identify the prepared hostname. Use Another
> person, login alex, and that hostname in Calamares. Check network access and
> Codex authentication. Identify the actual internal disk by model, serial and
> capacity; show me what will be deleted and ask me to confirm that disk and
> that needed files are backed up before any destructive command or Install.
> The intended plan is to erase the confirmed internal disk, use automatic
> partitioning, and install Plasma. Explain optional disk encryption and let
> me enter my own passwords privately. Open family-installer and help me
> through the graphical installer. Preserve the prepared host key and agent
> enrollment. Do not import Ivan's personal secrets, Git-sync or GPG keys.
> Check installation completion, then help me reboot from the internal disk
> and verify my account, T3 Code, Codex, and any prepared Tailscale support.

The USB bundle passphrase, disk-encryption passphrase, and login password have
different purposes. Keep them private. Installation erases the selected
internal disk; creating this USB already erased the USB. These are two separate
decisions. **Install** in Calamares starts writing immediately.

This media needs no call to Ivan to approve an SSH host key or re-encrypt the
included agent credentials. Network access and a valid provider subscription
are still required. If Tailscale enrollment was not included, local Codex
assistance still works. Do not infer that a running Tailscale daemon is enrolled.

After installation, boot the internal disk and sign in as the chosen user.
T3 Code and the prepared agent credentials are initialized at first boot.
The live agent conversation stays on the USB's ephemeral filesystem; save any
needed notes into the installed user's home before rebooting.

When Calamares reports success, reboot into the installed system before using
the agent again. The installer stops live Codex and transfers its latest credentials at the end
of installation. The agent disconnecting at that point is expected. Do not reuse the personal USB as an authenticated rescue disk.
