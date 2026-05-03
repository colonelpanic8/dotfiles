#!/usr/bin/env bash
set -euo pipefail

target_user="${TARGET_USER:-imalison}"
target_home="${TARGET_HOME:-/Users/${target_user}}"
dotfiles_checkout="${DOTFILES_CHECKOUT:-${target_home}/dotfiles}"

failures=0

check() {
  local description="$1"
  shift

  if "$@" >/dev/null 2>&1; then
    printf 'ok: %s\n' "$description"
  else
    printf 'missing: %s\n' "$description" >&2
    failures=$((failures + 1))
  fi
}

check "macOS user ${target_user} exists" id -u "$target_user"
check "home directory ${target_home} exists" sudo test -d "$target_home"
check "home directory is owned by ${target_user}" sudo sh -c '[ "$(stat -f "%Su" "$1")" = "$2" ]' sh "$target_home" "$target_user"
check "dotfiles checkout exists at ${dotfiles_checkout}" sudo test -d "$dotfiles_checkout/.git"
check "nix-darwin checkout exists under target home" sudo test -d "$dotfiles_checkout/nix-darwin"
check "SSH identity exists for agenix and git" sudo test -r "$target_home/.ssh/id_ed25519"
check "password store exists for pass/git-sync" sudo test -d "$target_home/.password-store"
check "org checkout exists for git-sync" sudo test -d "$target_home/org"

if [ "$failures" -ne 0 ]; then
  cat >&2 <<EOF

${target_user} is not ready for nix-darwin activation yet.

Create or rename the macOS login account first, migrate the home data into
${target_home}, then rerun:

  just check-imalison

Once every check passes, switch with:

  just switch-imalison
EOF
  exit 1
fi

printf '\n%s is ready for the imalison nix-darwin target.\n' "$target_user"
