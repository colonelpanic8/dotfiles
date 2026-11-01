# Tailscale enrollment profiles

NixOS and nix-darwin use one enrollment implementation in
[`../nix-shared/tailscale/`](../nix-shared/tailscale/). Each machine selects a
named profile; profiles are an extensible attribute set, not a fixed list of
people. Import the platform's `tailscale.nix` module and select enrollment in
that machine's definition:

```nix
imports = [../tailscale.nix];
myModules.tailscale.enrollment = "alex";
```

`myModules.tailscale.enable` controls the daemon and defaults to `true` when
this module is imported. `enrollment = null` (the default) leaves authentication
manual and provisions no enrollment credential. `enable = false` also disables
the module's enrollment job. WSL configurations retain their disabled daemon.

## Profiles and per-machine choices

The shared catalog is [`profiles.nix`](../nix-shared/tailscale/profiles.nix):

- `ivan`: the existing agenix `tailscale-authkey.age`, operator `imalison`,
  with expired-node reauthentication enabled for existing NixOS workstations.
- `alex`: separately provisioned `/var/lib/tailscale-alex/authkey`, operator
  `alex`, with initial enrollment only. No agenix import is needed.
- `kat`: the Neo's existing host-scoped encrypted key, operator `kat`, with
  initial enrollment only.

Workstations and Ivan's personal rescue stick explicitly select `ivan` in their
machine definitions. The Mini selects `ivan` with expired-node reauthentication
disabled to retain its bootstrap-only policy; the Neo selects `kat`. New Alex
machines can opt into `alex`. The Air retains its existing Tailscale.app connection.
The generic nixos-portable stick and installer are independent of this catalog.

Add another profile here or in a machine module, then select it:

```nix
myModules.tailscale = {
  enrollment = "another-tailnet";
  enrollmentProfiles.another-tailnet = {
    authKeyFile = "/var/lib/tailscale-enrollment/another-tailnet/authkey";
    operator = "another-user"; # null omits operator access
    acceptDns = true;
    advertiseTags = ["tag:family-pc"]; # optional, authorized by the key
    reauthenticateExpired = false;   # default
  };
};
```

Alternatively, use `authKeySecret = ./secrets/another-tailnet.age` and import
agenix for the target platform. Specify exactly one credential source. An
`authKeyFile` is a **quoted runtime string**, never a Nix path, `builtins.readFile`,
inline key, or `environment.etc.*.text` containing the key. `authKeySecret` is
only for an encrypted file; only the selected profile becomes an agenix secret.
Unselected profiles do not add decryption identities or provision credentials.
A machine can override catalog fields, including a host-specific encrypted key:

```nix
myModules.tailscale.enrollment = "alex";
myModules.tailscale.enrollmentProfiles.alex = {
  authKeyFile = null;
  authKeySecret = ../../nixos/secrets/tailscale-authkey.alexanders-macbook-air.age;
};
age.identityPaths = ["/etc/ssh/ssh_host_ed25519_key"];
```

Profile names are local configuration labels. The credential determines the
actual tailnet; the hostname and Unix username do not. Changing the selector
does not migrate an already enrolled node to another tailnet. Review and perform
such a migration separately. Do not combine profile enrollment with upstream
`services.tailscale.authKeyFile` on NixOS.

## Existing machines and retry behavior

Preserve the existing SSH connection and `/var/lib/tailscale` identity during
adoption. Do not log out, erase state, or copy another computer's state. Build
and review before activating. NixOS activation uses `just switch` from the
primary `/srv/dotfiles/nixos` checkout. Retain machine-level restart safeguards for Tailscale and NetworkManager when
adopting this configuration over a remote connection.

The shared job checks status before opening a credential. By default it enrolls
only a new `NeedsLogin` node without an existing node ID, assigned address, or
tailnet. Running nodes, existing expired logins, approval-pending nodes, stopped
nodes, and unknown/starting states remain unchanged. A missing bootstrap key
does not invalidate a persisted login. No logout, reset, or daemon restart is
performed by the enrollment script.

`reauthenticateExpired = true` explicitly permits `--force-reauth` when a Running
or NeedsLogin node reports a node-key expiry in the past. Use a valid renewable
credential for the same tailnet. This preserves Ivan's NixOS expiry-repair policy;
new profiles default to bootstrap-only behavior. Stopped and approval-pending
nodes are left alone even with expiry repair enabled.

NixOS runs the job at boot and retries after two minutes, then every thirty
minutes. macOS runs a launchd enrollment job at load and every five minutes;
its existing separate job still reconciles DNS acceptance, hostname, and operator
preferences. Missing, empty, or `DISABLED` credentials skip enrollment. Rejected
credentials or unsafe file permissions report failure without stopping Tailscale.
Auth command output is suppressed so login URLs and credentials stay out of logs.

## Credentials and provisioning

An administrator of the intended tailnet must supply its credential. A
machine-specific, non-ephemeral, single-use key suits bootstrap-only profiles.
Pre-approve it if unattended enrollment requires device approval. Untagged keys
retain user ownership; tags create tag-owned devices. Auth-key expiration is
separate from persisted node-key expiration. A consumed single-use key cannot
provide indefinite automatic recovery after state loss or node-key expiry.

For fleet provisioning, generate fresh keys on a trusted provisioning system
using a narrowly scoped OAuth client from the intended tailnet. This module takes
an auth key, never the long-lived OAuth client secret. Keep that secret off
family PCs. Tags and auth keys do not establish network isolation; configure the
intended support grants/ACLs and review existing broad allow rules separately.

Provision runtime files over a private authenticated channel. Create the parent
directory as root with mode `0700`; the auth key must be a root-owned regular file
with mode `0400` or `0600`. Independently decrypted runtime symlinks work too.
Only the file path appears in command arguments and Nix. Never paste keys into
chat or shell history. For encrypted profiles, grant only the intended secret
to the verified host key. Do not add family hosts to broad workstation
`hostKeys`, `sshClientKeys`, or `agenixKeys` recipient groups.

## Inspection and local validation

On NixOS:

```sh
tailscale status
systemctl status tailscale-autoconnect.service tailscale-autoconnect.timer
journalctl -u tailscale-autoconnect.service --no-pager
```

On macOS, inspect `/var/log/tailscale-enroll.err.log`. Verify SSH from the intended
helper after activation and after a planned boot. Local checks use fake credentials
and a fake CLI; they perform no live enrollment, activation, or reboot:

```sh
python3 nixos/checks/tailscale-enrollment.py
nix eval --json --impure --expr 'import ./nixos/checks/tailscale-module.nix { nixpkgs = (builtins.getFlake (toString ./nix-darwin)).inputs.nixpkgs; }'
nix eval --json --impure --expr 'let f = builtins.getFlake (toString ./nix-darwin); in import ./nixos/checks/tailscale-module.nix { nixpkgs = f.inputs.nixpkgs; agenix = f.inputs.agenix; darwin = f.inputs.nix-darwin; }'
```

Sources: [Tailscale auth keys](https://tailscale.com/docs/features/access-control/auth-keys),
[`tailscale up`](https://tailscale.com/docs/reference/tailscale-cli/up),
[OAuth clients](https://tailscale.com/kb/1215/oauth-clients),
[family installer operating guide](https://github.com/colonelpanic8/nixos-portable/blob/master/README.md).
