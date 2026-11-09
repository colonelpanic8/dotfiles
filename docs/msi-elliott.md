# msi-elliott

Alex's MSI desktop uses `nixos/machines/msi-elliott.nix`, which also holds its
hardware scan, NVIDIA driver, and the settings carried over from the original
installed configuration. The primary account is `alex`, and the existing disk
layout is retained.

## Build and switch

This machine uses the repository-root standalone flake. It pins the installed
NixOS 26.05 revision and Home Manager's corresponding release, and builds on
`nixos/profiles/standalone.nix` (the shared shell environment) and
`nixos/profiles/standalone-desktop.nix` (SDDM, Hyprland with DMS, Plasma 6, and
the desktop application set). It does not evaluate the workstation
flake's private inputs or import its personal accounts and secrets.

```sh
cd /srv/dotfiles
nix build .#nixosConfigurations.msi-elliott.config.system.build.toplevel
cd nixos
just switch
```

The system exports `DOTFILES_SYSTEM_FLAKE=/srv/dotfiles#msi-elliott`, which the
normal `just switch` recipe uses. Activate only from this primary checkout.
Keep `system.stateVersion` at `26.05` when updating inputs.

Alex gets the shared Zsh environment, Starship, Git, GitHub CLI, tmux, Zellij,
direnv, Neovim, and common command-line tools. Home Manager links the shared
tmux, Zellij, inputrc, dircolors, and ignore files. Git uses Alex's identity,
and only Alex receives passwordless sudo.

## Power

The machine must stay reachable, so every suspend path is disabled: systemd
refuses suspend, hibernate, and hybrid sleep and masks their targets; logind
ignores idle, lid, and sleep keys; GNOME's idle sleep is off; and
`/etc/xdg/powerdevilrc` locks Plasma's idle, lid, and power-button actions so
they cannot sleep, even if changed in System Settings.

## Identity and access

The user Ed25519 key was generated on the desktop. Its private half remains
in `/home/alex/.ssh/id_ed25519`; its public half is in `alexKeys` in
`nix-shared/keys.nix` and is registered with GitHub account `alexmalison`.
Fingerprint: `SHA256:W496ViNPlJa+HEVaE0JSh8oRYaWtRnbAUKCWVbJH3f8`.

The authorized inbound support and Alex keys are declared using `userKeys.alex`.
These inbound permissions do not supply Ivan's credentials to this machine.
Do not add this host or user identity to the broad workstation secret recipient
groups or copy another person's credentials to resolve an authentication error.
Its GitHub CLI token and Tailscale auth key are agenix secrets encrypted only to
this host's SSH key.

See [Backup SSH for msi-elliott](msi-elliott-backup-access.md) for the Upterm
relay that works without Tailscale.

## Connectivity

Tailscale and NetworkManager are enabled at boot and are not restarted by
activation. Package upgrades therefore require an intentionally scheduled
restart to run the new daemon binaries.

The machine uses the shared `alex` enrollment profile
(`myModules.tailscale.enrollment = "alex"`) with its host-scoped auth key. The
enrollment service skips a node that is already logged in, so the existing node
ID and addresses are retained. See [Tailscale](../nixos/tailscale.md).

The bootstrap activation scripts under `/home/alex/.cache/dotfiles-bootstrap/`
start `just switch` in a system service and arm a 15-minute rollback. After a
fresh SSH connection, checking the active generation, and confirming the same
Tailscale node ID and address, cancel the timer:

```sh
sudo systemctl stop dotfiles-connectivity-rollback.timer
```

Reboot behavior must be tested separately while someone can recover locally; a
transient rollback timer does not protect against a boot failure.
