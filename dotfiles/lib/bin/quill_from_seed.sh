#!/usr/bin/env bash

encrypted_seed_file="$1"

shift

gpg --decrypt "$encrypted_seed_file"  2>/dev/null | keysmith private-key -f - -o - | quill --pem-file - "$@"
