set shell := ["bash", "-lc"]

# Repo-level helpers.
#
# NixOS workflows live under `nixos/justfile`, but it's useful to have a
# top-level command for populating the Cachix cache from a local machine.

cachix_cache := "colonelpanic8-dotfiles"
nixos_dir := "nixos"
railbird_secrets_stub := "nixos/ci/railbird-secrets-stub"

# Build the NixOS system closure for `host` and push any new /nix/store paths to Cachix.
#
# Prereqs:
# - Cachix auth configured (either `cachix authtoken ...` or `CACHIX_AUTH_TOKEN` in env)
#
# Usage:
# - `just cachix-populate` (defaults to host=strixi-minaj)
# - `just cachix-populate host=railbird-sf`
cachix-populate host="strixi-minaj":
  set -euo pipefail
  command -v cachix >/dev/null
  command -v nix >/dev/null

  mapfile -t outs < <(
  nix build \
    --no-link \
    --print-build-logs \
    --print-out-paths \
    ./{{nixos_dir}}#nixosConfigurations.{{host}}.config.system.build.toplevel \
    --override-input railbird-secrets ./{{railbird_secrets_stub}}
  )

  cachix push {{cachix_cache}} "${outs[@]}"

# Configure Cachix auth token from the clipboard (Wayland or X11), without echoing it.
#
# Usage:
# - Copy the token from Cachix UI
# - Run `just cachix-auth-from-clipboard`
cachix-auth-from-clipboard:
  set -euo pipefail
  command -v cachix >/dev/null

  if command -v wl-paste >/dev/null; then wl-paste --no-newline | cachix authtoken --stdin; printf '' | wl-copy; \
  elif command -v xclip >/dev/null; then xclip -o -selection clipboard | tr -d '\n' | cachix authtoken --stdin; printf '' | xclip -selection clipboard; \
  else echo "No clipboard tool found (expected wl-paste or xclip)." >&2; exit 1; fi
