#!/usr/bin/env bash
mkdir -p /var/log/gitea-runner/
chown -R $1 /var/log/gitea-runner
chmod 755 /var/log/gitea-runner

mkdir -p /var/lib/gitea-runner/nix
chown -R $1 /var/lib/gitea-runner
chmod 755 /var/lib/gitea-runner
