# Run as root, with Git on PATH. Keep this usable before the first rebuild.
set -euo pipefail

checkout=$1
shift

if ! /usr/bin/dscl . -read /Groups/dotfiles >/dev/null 2>&1; then
  # Let macOS allocate an unused GID instead of reserving a fleet-wide number.
  /usr/sbin/dseditgroup -o create -r 'Dotfiles collaborators' dotfiles
fi
for member in "$@"; do
  /usr/sbin/dseditgroup -o edit -a "$member" -t user dotfiles
done

# A first activation may precede cloning the shared checkout. Do not create an
# empty repository or change a symlink target outside the shared location.
if [ -L "$checkout" ] || [ ! -d "$checkout/.git" ]; then
  echo "Skipping dotfiles permissions: $checkout is not a standalone checkout" >&2
  exit 0
fi

# Physical traversal leaves symlink targets (including Nix store paths) alone.
/usr/bin/find -P -x "$checkout" \( -type d -o -type f \) \
  -exec /usr/bin/chgrp dotfiles {} + \
  -exec /bin/chmod g+rwX {} +
/usr/bin/find -P -x "$checkout" -type d -exec /bin/chmod g+s {} +

# Inherit write access even when editors use umask 022. Search permission is
# inherited by directories only, so ordinary files do not become executable.
# chmod +a merges existing entries for this group, making repeated runs safe.
/usr/bin/find -P -x "$checkout" \( -type d -o -type f \) -exec /bin/chmod +a \
  'group:dotfiles allow read,write,append,delete,readattr,writeattr,readextattr,writeextattr,readsecurity,file_inherit,directory_inherit' {} +
/usr/bin/find -P -x "$checkout" -type d -exec /bin/chmod +a \
  'group:dotfiles allow search,directory_inherit' {} +

git -c safe.directory="$checkout" -C "$checkout" config core.sharedRepository group
