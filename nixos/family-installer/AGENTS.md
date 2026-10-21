# Agent instructions

Read [README.md](README.md) before assisting with installation, remote support,
registration, updates, or recovery. It is the operating guide for both the
person at the computer and an assisting agent. Start with its
[agent instructions](README.md#instructions-for-an-assisting-agent), then use
the section for the current stage. Establish which computer and environment
you are controlling before running commands.

Alex's current workflow starts on his MacBook Air, with its existing dotfiles
clone and a prepared base ISO. Read [START-HERE.md](START-HERE.md) for the live
USB prompt. The old Windows release instructions describe a generic fallback.

On combined rescue media, this guide and README are installed under
`/etc/nixos-family-installer`. On a family target, the source copy is under
`/srv/nixos-family-installer`. Follow local machine instructions as well.

## Project scope

This directory builds a Calamares NixOS installer inside dotfiles. Its flake
is self-contained and never evaluates the parent workstation flake. The target's
account comes from Calamares; owner (`ivan` / `other`) is independent of the
login name.

## Structure

- `modules/iso.nix`: standalone graphical media.
- `modules/calamares.nix`: reusable installer, package override, and support guide.
- `calamares/owner.conf`: owner selection page.
- `scripts/patch_calamares.py`: narrow, checked patch against pinned upstream.
- `scripts/prepare_target.py`: copy release source, preserve generated hardware
  and boot configuration, create permanent host identity and public request.
- `machines/<hostname>/`: reviewed registrations used by managed updates.
- `modules/user-profile.nix`: safe shared user preferences.

## Invariants

- Never import `../configuration.nix` into a family installation.
- Owner selection does not grant secret access. Do not import personal GPG,
  Git-sync, password-store, Syncthing or agent credentials implicitly.
- Keep Calamares' partition/encryption, user password, and bootloader handling.
- A personal target key may be generated on the trusted preparation computer
  and transported in the encrypted personal bundle. Never put it in Git, a
  Nix input, a public ISO, or logs. Enrollment requests contain public data.
- Only explicitly selected provider accounts are provisioned. Codex-only
  media is supported; do not require Claude login when it was omitted.
- Confirm USB identity before writing media and internal disk identity before
  installation. These are separate destructive operations.
- Do not enable automatic updates until the host exists on the published
  `family-stable` ref. Never reboot unattended by default.
- For dotfiles development, use only the primary `/srv/dotfiles` checkout.
  Follow its root AGENTS.md and `nixos/AGENTS.md`. Do not create a Git worktree.
  On an installed family computer, edit its local configuration as documented.
- Keep this flake self-contained. The target copies this directory, so imports
  outside it would break installed configurations.
- Managed updates select dotfiles with `dir=nixos/family-installer` and the
  manually promoted `family-stable` ref.

## Development

Use Nix with flakes and `direnv allow`. No submodule checkout is needed.
Run `direnv exec . just check`, then `nix flake check` to build checks. `just build` builds the ISO; `just vm` boots it in a disposable VM.
An ISO build is not proof that installation succeeds: verify installation,
reboot, primary account and exclusions in the VM before shipping media.

Only format this directory and other files changed for the task.
