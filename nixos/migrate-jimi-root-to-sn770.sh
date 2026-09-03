#!/usr/bin/env bash
set -euo pipefail

# Offline root migration for jimi-hendnix. Run as root only after booting the
# SATA NixOS installation whose root filesystem UUID is listed below.

readonly RESCUE_ROOT_UUID="30583504-9530-4095-a556-da1209ef9b63"
readonly SOURCE_SERIAL="PHNH2114027E1P0B"
readonly TARGET_SERIAL="233216802763"
readonly TARGET_ROOT_UUID="8f024ed1-116e-48cf-a229-1d6aaf978cad"
readonly TARGET_BOOT_ID="A19C7D42"
readonly TARGET_BOOT_UUID="A19C-7D42"
readonly TARGET_DISK_GUID="9c77db55-204d-4482-8b25-e2b260e8ef6f"
readonly TARGET_BOOT_PART_GUID="706a64e6-e3e9-4be2-b90e-2dafe6729dc6"
readonly SYSTEM_CLOSURE="/nix/store/fc07vz5wn4gbaqmb33siw6h9hykxlcfz-nixos-system-jimi-hendnix-26.11.20260718.61b7c44"

die() {
  echo "error: $*" >&2
  exit 1
}

partition_by_number() {
  local disk="$1"
  local number="$2"
  lsblk -nrpo NAME,PARTN "$disk" | awk -v number="$number" '$2 == number { print $1; exit }'
}

disk_by_serial() {
  local serial="$1"
  lsblk -dnpo NAME,SERIAL | awk -v serial="$serial" '$2 == serial { print $1; exit }'
}

[[ $EUID -eq 0 ]] || die "run this script as root"
[[ $SYSTEM_CLOSURE == /nix/store/* ]] || die "the migration system closure was not embedded"

running_root="$(readlink -f "$(findmnt -nro SOURCE /)")"
running_root_uuid="$(blkid -s UUID -o value "$running_root")"
[[ $running_root_uuid == "$RESCUE_ROOT_UUID" ]] ||
  die "refusing to run: / is UUID $running_root_uuid, not the SATA rescue root"

source_disk="$(disk_by_serial "$SOURCE_SERIAL")"
target_disk="$(disk_by_serial "$TARGET_SERIAL")"
[[ -n $source_disk ]] || die "Intel source disk serial $SOURCE_SERIAL was not found"
[[ -n $target_disk ]] || die "SN770 target disk serial $TARGET_SERIAL was not found"
[[ $source_disk != "$target_disk" ]] || die "source and target resolved to the same disk"

source_root="$(partition_by_number "$source_disk" 2)"
target_root="$(partition_by_number "$target_disk" 1)"
target_swap="$(partition_by_number "$target_disk" 2)"
target_efi="$(partition_by_number "$target_disk" 3)"
[[ -b $source_root && -b $target_root && -b $target_swap && -b $target_efi ]] ||
  die "expected NVMe partitions were not found"

echo "Rescue root: $running_root ($running_root_uuid)"
echo "Source:      $source_disk ($SOURCE_SERIAL), root $source_root"
echo "TARGET:      $target_disk ($TARGET_SERIAL), root $target_root, EFI $target_efi"
echo
echo "This will permanently erase partitions 1 and 3 on the SN770."
read -r -p "Type 'erase SN770' to continue: " confirmation
[[ $confirmation == "erase SN770" ]] || die "confirmation did not match"

for device in "$source_root" "$target_root"; do
  if findmnt -rn -S "$device" >/dev/null; then
    die "$device is already mounted"
  fi
done

target_efi_mount="$(findmnt -rn -S "$target_efi" -o TARGET || true)"
if [[ -n $target_efi_mount ]]; then
  echo "Unmounting target EFI partition from $target_efi_mount"
  umount "$target_efi_mount"
fi

if swapon --noheadings --raw --show=NAME | grep -Fxq "$target_swap"; then
  echo "Disabling swap on $target_swap while its GPT is updated"
  swapoff "$target_swap"
fi

echo "Giving the SN770 a unique GPT disk ID and EFI partition ID"
sfdisk --disk-id "$target_disk" "$TARGET_DISK_GUID"
sfdisk --part-uuid "$target_disk" 3 "$TARGET_BOOT_PART_GUID"
partprobe "$target_disk"
udevadm settle

echo "Formatting the SN770 root and EFI partitions"
mkfs.ext4 -F -L nixos-root -U "$TARGET_ROOT_UUID" "$target_root"
mkfs.fat -F 32 -n NIXOS-BOOT -i "$TARGET_BOOT_ID" "$target_efi"
udevadm settle

source_mount="$(mktemp -d /mnt/jimi-source.XXXXXX)"
target_mount="$(mktemp -d /mnt/jimi-target.XXXXXX)"

cleanup() {
  set +e
  mountpoint -q "$target_mount/boot" && umount "$target_mount/boot"
  mountpoint -q "$target_mount" && umount "$target_mount"
  mountpoint -q "$source_mount" && umount "$source_mount"
  rmdir "$target_mount" "$source_mount" 2>/dev/null || true
}
trap cleanup EXIT

mount -o ro "$source_root" "$source_mount"
mount "$target_root" "$target_mount"

rsync_bin="$(command -v rsync || true)"
if [[ -z $rsync_bin ]]; then
  rsync_bin="$(find /nix/store -path '*/bin/rsync' -type f -print -quit)"
fi
[[ -x $rsync_bin ]] || die "rsync is not available on the SATA installation"

echo "Copying the Intel root filesystem to the SN770"
"$rsync_bin" -aHAXSx --numeric-ids --info=progress2 \
  --exclude='/tmp/*' \
  --exclude='/var/tmp/*' \
  --exclude='/mnt/*' \
  "$source_mount/" "$target_mount/"

mkdir -p "$target_mount/boot"
mount "$target_efi" "$target_mount/boot"

[[ -e "$target_mount$SYSTEM_CLOSURE" ]] ||
  die "prebuilt system closure is missing from the copied target"

echo "Installing the prebuilt NixOS system and systemd-boot onto the SN770"
nixos-install \
  --root "$target_mount" \
  --system "$SYSTEM_CLOSURE" \
  --no-root-password \
  --no-channel-copy

sync
echo
echo "Migration completed. The Intel root was left untouched as a rollback copy."
echo "Reboot and select the SN770 Linux Boot Manager entry if firmware does not choose it automatically."
