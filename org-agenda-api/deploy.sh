#!/usr/bin/env bash
set -euo pipefail

# Deploy customized org-agenda-api container to Fly.io
# Usage: ./deploy.sh <instance> [flyctl deploy args...]
# Example: ./deploy.sh colonelpanic
#          ./deploy.sh kat

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
NIXOS_DIR="$SCRIPT_DIR/../nixos"
cd "$SCRIPT_DIR"

# Parse instance argument
INSTANCE="${1:-}"
if [[ -z "$INSTANCE" ]]; then
  echo "Usage: $0 <instance> [flyctl deploy args...]"
  echo "Available instances:"
  for dir in configs/*/; do
    echo "  - $(basename "$dir")"
  done
  exit 1
fi
shift

CONFIG_DIR="$SCRIPT_DIR/configs/$INSTANCE"
if [[ ! -d "$CONFIG_DIR" ]]; then
  echo "Error: Instance '$INSTANCE' not found in configs/"
  exit 1
fi

# Source instance configuration
if [[ -f "$CONFIG_DIR/config.env" ]]; then
  source "$CONFIG_DIR/config.env"
else
  echo "Error: $CONFIG_DIR/config.env not found"
  exit 1
fi

echo "Deploying instance: $INSTANCE"
echo "  Fly app: $FLY_APP"

# Check for uncommitted changes
if [[ -n "$(git status --porcelain)" ]]; then
  echo ""
  echo "WARNING: Working directory has uncommitted changes!"
  echo "For reproducibility, consider committing before deploying."
  echo ""
  read -p "Continue anyway? [y/N] " -n 1 -r
  echo
  if [[ ! $REPLY =~ ^[Yy]$ ]]; then
    exit 1
  fi
fi

# Get input revisions for reproducibility from the nixos flake
ORG_API_NODE=$(jq -r '.nodes.root.inputs."org-agenda-api"' "$NIXOS_DIR/flake.lock")
ORG_API_REV=$(jq -r ".nodes.\"$ORG_API_NODE\".locked.rev" "$NIXOS_DIR/flake.lock")
# For dotfiles rev, use the current git commit since we're building from local
DOTFILES_REV=$(git -C "$NIXOS_DIR/.." rev-parse HEAD)

SHORT_DOTFILES="${DOTFILES_REV:0:7}"
SHORT_ORG_API="${ORG_API_REV:0:7}"

echo "Versions:"
echo "  org-agenda-api: $ORG_API_REV"
echo "  dotfiles:       $DOTFILES_REV"

# Build container from nixos flake for this instance
# Use --refresh to ensure we're not using stale cached builds
echo "Building container from flake..."
nix build "$NIXOS_DIR#container-$INSTANCE" -o "result-container-$INSTANCE" --refresh

# Load into Docker
echo "Loading container into Docker..."
LOADED_IMAGE=$(docker load < "result-container-$INSTANCE" 2>&1 | grep -oP 'Loaded image: \K.*')
echo "Loaded: $LOADED_IMAGE"

# Tag with both versions for full reproducibility
# Format: api-<org-api-rev>-cfg-<dotfiles-rev>
IMAGE_TAG="api-${SHORT_ORG_API}-cfg-${SHORT_DOTFILES}"
IMAGE_NAME="registry.fly.io/$FLY_APP:$IMAGE_TAG"
echo "Tagging as $IMAGE_NAME..."
docker tag "$LOADED_IMAGE" "$IMAGE_NAME"

echo "Pushing to Fly.io registry..."
flyctl auth docker
docker push "$IMAGE_NAME"

# Decrypt secrets
echo "Decrypting secrets..."

IDENTITY=""
for key_type in ed25519 rsa; do
  if [[ -f "$HOME/.ssh/id_${key_type}" ]]; then
    IDENTITY="$HOME/.ssh/id_${key_type}"
    break
  fi
done

if [[ -z "$IDENTITY" ]]; then
  echo "Error: No SSH identity found" >&2
  exit 1
fi

GIT_SSH_KEY=$(age -d -i "$IDENTITY" "$CONFIG_DIR/secrets/git-ssh-key.age")
AUTH_PASSWORD=$(age -d -i "$IDENTITY" "$CONFIG_DIR/secrets/auth-password.age")

echo "Setting Fly.io secrets..."

SECRET_ARGS=(
  "GIT_SSH_PRIVATE_KEY=$GIT_SSH_KEY"
  "AUTH_USER=$AUTH_USER"
  "AUTH_PASSWORD=$AUTH_PASSWORD"
  "GIT_USER_EMAIL=$GIT_USER_EMAIL"
  "GIT_USER_NAME=$GIT_USER_NAME"
)

# Use GIT_SYNC_REPOSITORIES (multi-repo) or GIT_SYNC_REPOSITORY (single repo)
if [[ -n "${GIT_SYNC_REPOSITORIES:-}" ]]; then
  SECRET_ARGS+=("GIT_SYNC_REPOSITORIES=$GIT_SYNC_REPOSITORIES")
elif [[ -n "${GIT_SYNC_REPOSITORY:-}" ]]; then
  SECRET_ARGS+=("GIT_SYNC_REPOSITORY=$GIT_SYNC_REPOSITORY")
else
  echo "Error: Neither GIT_SYNC_REPOSITORIES nor GIT_SYNC_REPOSITORY set in config.env"
  exit 1
fi

flyctl secrets set "${SECRET_ARGS[@]}" --stage -a "$FLY_APP"

echo "Deploying $IMAGE_NAME..."
flyctl deploy --image "$IMAGE_NAME" -c "$CONFIG_DIR/fly.toml" "$@"

# Cleanup
rm -f "result-container-$INSTANCE"

echo ""
echo "Done! Deployed to $FLY_APP"
echo "  Image: $IMAGE_NAME"
echo "  org-agenda-api: $ORG_API_REV"
echo "  dotfiles:       $DOTFILES_REV"
