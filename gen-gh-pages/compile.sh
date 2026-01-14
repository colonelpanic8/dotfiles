#!/usr/bin/env bash
# Local compilation script for generating HTML from README.org
# Note: For CI, this is now handled by GitHub Actions (.github/workflows/gh-pages.yml)

set -e

THIS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# Use system emacs or EMACS environment variable
EMACS="${EMACS:-emacs}"

cd "$THIS_DIR"

# Install cask dependencies if needed
if [ ! -d ".cask" ]; then
    cask install
fi

# Generate HTML
cask exec "$EMACS" --script generate-html.el

# Move the generated file
mv "$THIS_DIR/../dotfiles/emacs.d/README.html" .

echo "Generated README.html in $THIS_DIR"
