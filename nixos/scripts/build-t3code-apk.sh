#!/usr/bin/env bash
# Build a sideloadable Android APK from the personal patched T3 Code source.
#
# The desktop patch stack in nix-shared/t3code.nix already produces
# `t3codePatchedSource` (upstream pin + every carried PR diff, including the
# mobile-relevant composer fixes). We reuse that exact store path as the build
# tree so the phone build can never drift from the desktop build, then run the
# T3 Code repo's own `.#android` dev shell (Node 24, JDK 17, Android SDK/NDK,
# CMake, Zig) against a writable copy of it.
#
# The result is a debug-signed release APK suitable for sideloading (adb
# install), not for Play Store upload. See apps/mobile/README.md "Android on
# NixOS".
#
# Env overrides:
#   T3CODE_APK_WORKDIR  writable build tree (default ~/.cache/t3code-apk-build)
#   T3CODE_APK_OUT      output dir for the copied APK (default ~/t3code-apk)
#   T3CODE_APK_VARIANT  APP_VARIANT for the build (default production)
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
flake_dir="$(cd "$here/.." && pwd)" # /srv/dotfiles/nixos
host="$(hostname)"

workdir="${T3CODE_APK_WORKDIR:-$HOME/.cache/t3code-apk-build}"
outdir="${T3CODE_APK_OUT:-$HOME/t3code-apk}"
variant="${T3CODE_APK_VARIANT:-production}"
# Only build the target device's ABI by default. Building all four ABIs
# (arm64-v8a, armeabi-v7a, x86, x86_64) in parallel roughly quadruples the
# native CMake/NDK work and peak memory; modern phones are arm64-v8a. Set
# T3CODE_APK_ABIS="arm64-v8a,x86_64" to widen (e.g. to also cover an emulator).
abis="${T3CODE_APK_ABIS:-arm64-v8a}"

log() { printf '\n\033[1;34m==> %s\033[0m\n' "$*"; }

log "Building patched T3 Code source via Nix (reuses desktop build cache)"
src="$(nix build --no-link --print-out-paths \
  "${flake_dir}#nixosConfigurations.${host}.pkgs.t3code.unwrapped.src")"
echo "patched source: $src"

# A full React Native + Gradle + NDK build plus node_modules is large; warn if
# the filesystem holding the work dir is tight rather than failing deep into
# gradle.
avail_kb="$(df -Pk "$(dirname "$workdir")" | awk 'NR==2 {print $4}')"
if [ "${avail_kb:-0}" -lt 20971520 ]; then
  printf '\033[1;33mWARNING: only %s GiB free near %s; RN/Gradle builds can need 15-20 GiB.\033[0m\n' \
    "$((avail_kb / 1048576))" "$workdir" >&2
fi

log "Refreshing writable build tree at $workdir"
rm -rf "$workdir"
mkdir -p "$workdir"
# -a preserves the tree; the store copy is read-only, so make it writable.
cp -a "$src"/. "$workdir"/
chmod -R u+w "$workdir"

log "Building release APK inside the .#android dev shell (this takes a while)"
# The Android toolchain comes from the vendored standalone flake (upstream
# T3 Code does not commit a flake.nix, and the github-tarball source input has
# none). It is a pure toolchain shell independent of the source, so we point
# `nix develop` at it while running the build from the patched work tree; this
# also avoids nix copying the multi-GiB work tree into the store to evaluate a
# flake. EXPO_NO_GIT_STATUS is required because the work tree is not a git
# checkout.
cd "$workdir"
nix develop "${flake_dir}/t3code-android#android" --command bash -euo pipefail -c '
  # Force a project-local virtual store. This machine has pnpm'"'"'s global
  # virtual store enabled, which only symlinks packages from a shared location;
  # Gradle/Expo module resolution (e.g. require("@expo/config-plugins") during
  # :expo-constants:createExpoConfig) then fails because the copied work tree
  # has no real local node_modules layout.
  CI=true corepack pnpm install --frozen-lockfile --config.enableGlobalVirtualStore=false
  cd apps/mobile
  APP_VARIANT="'"$variant"'" EXPO_NO_GIT_STATUS=1 expo prebuild --clean --platform android
  cd android
  # Restrict ABIs and cap parallelism to keep peak memory down: this host has a
  # known RAM fault that surfaces as JVM G1 GC SIGSEGV crashes under heavy
  # parallel native compilation. --no-daemon avoids a lingering large-heap JVM.
  # Skip the release "lint vital" tasks: they are not needed for a sideload
  # build and the current AGP/JDK combo aborts them with "Unexpected lint
  # invalid arguments".
  NODE_ENV=production ./gradlew assembleRelease \
    -PreactNativeArchitectures="'"$abis"'" \
    -x lintVitalAnalyzeRelease -x lintVitalReportRelease -x lintVitalRelease \
    --no-daemon --max-workers=4
'

apk="$workdir/apps/mobile/android/app/build/outputs/apk/release/app-release.apk"
if [ ! -f "$apk" ]; then
  echo "ERROR: expected APK not found at $apk" >&2
  exit 1
fi

mkdir -p "$outdir"
stamp="$(basename "$src" | sed 's/^t3code-patched-//')"
dest="$outdir/t3code-${variant}-${stamp}.apk"
cp -f "$apk" "$dest"

log "APK ready"
echo "$dest"
echo
echo "Sideload with:  adb install -r \"$dest\""
