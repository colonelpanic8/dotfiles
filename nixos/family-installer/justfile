default:
    @just --list

fmt:
    alejandra flake.nix modules
    ruff format scripts tests calamares

fmt-check:
    alejandra --check flake.nix modules
    ruff format --check scripts tests calamares

lint:
    ruff check scripts tests calamares

test:
    PYTHONPATH=scripts python -m unittest discover -s tests -v

check: fmt-check lint test
    nix flake check --no-build

build:
    nix build .#iso

prepare *args:
    python scripts/prepare_media.py --source . {{args}}

vm:
    #!/usr/bin/env bash
    set -euo pipefail
    iso=$(find -L result/iso -maxdepth 1 -name '*.iso' -print -quit)
    test -n "$iso"
    disk=$(mktemp --suffix=.qcow2)
    qemu-img create -f qcow2 "$disk" 40G
    acceleration=tcg
    if [ -r /dev/kvm ] && [ -w /dev/kvm ]; then acceleration=kvm; fi
    echo "VM disk: $disk"
    qemu-system-x86_64 -accel "$acceleration" -m 4096 -smp 4 \
      -drive "file=$disk,format=qcow2,if=virtio" -cdrom "$iso" -boot d \
      -nic user,model=virtio-net-pci
