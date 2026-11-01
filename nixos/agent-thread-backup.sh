# Usage: agent-thread-backup NAME=SSH_TARGET...
#
# Pulls T3 Code, Paseo, Claude Code, Codex, and opencode thread history from
# each host into $AGENT_THREAD_BACKUP_DIR/NAME. Transcripts are mirrored
# without --delete, so files a host prunes stay in the backup. SQLite stores
# are snapshotted on the host, then kept as dated zstd copies.

dest_root="${AGENT_THREAD_BACKUP_DIR:-$HOME/Backups/agent-threads}"
keep_snapshots="${AGENT_THREAD_BACKUP_KEEP:-14}"
local_host="$(uname -n)"
local_host="${local_host%%.*}"
today="$(date +%F)"

# Runs on each host under its own bash (macOS ships 3.2). Prints the
# home-relative directories to mirror.
read -r -d '' remote_prepare <<'EOF' || true
cd "$HOME" || exit 1
stage=.cache/agent-thread-backup
rm -rf "$stage"
mkdir -p "$stage" || exit 1
snapshot() {
  [ -f "$1" ] || return 0
  python3 -I -c '
import sqlite3, sys
src = sqlite3.connect(sys.argv[1], timeout=60)
dst = sqlite3.connect(sys.argv[2])
src.backup(dst)
dst.close()
src.close()
' "$1" "$stage/$2" || echo "snapshot of $1 failed" >&2
}
snapshot .t3/userdata/statev2.sqlite t3-statev2.sqlite
snapshot .local/share/opencode/opencode-stable.db opencode.sqlite
for d in .claude*/projects .codex*/sessions .codex*/archived_sessions \
  .paseo/agents .paseo/projects .paseo/schedules .paseo/loops \
  .paseo/creations .paseo/agent-requests; do
  [ -d "$d" ] && printf '%s\n' "$d"
done
exit 0
EOF

control_dir="$(mktemp -d)"
trap 'rm -rf "$control_dir"' EXIT
ssh_opts=(
  -o BatchMode=yes
  -o ConnectTimeout=15
  -o ServerAliveInterval=30
  -o ControlMaster=auto
  -o "ControlPath=$control_dir/%C"
  -o ControlPersist=120
)

backup_host() {
  local name="$1" target="$2"
  local host_dir="$dest_root/$name"
  local -a run
  local src dirs d f snapshot_dir

  if [[ "$name" == "$local_host" ]]; then
    run=(bash -s)
    src="$HOME/"
  else
    run=(ssh "${ssh_opts[@]}" "$target" bash -s)
    src="$target:"
  fi

  dirs="$("${run[@]}" <<<"$remote_prepare")" || return 1

  while IFS= read -r d; do
    [[ -n "$d" ]] || continue
    rsync -a --mkpath -e "ssh ${ssh_opts[*]}" \
      "$src$d/" "$host_dir/home/$d/" || return 1
  done <<<"$dirs"

  rsync -a --mkpath -e "ssh ${ssh_opts[*]}" \
    "$src.cache/agent-thread-backup/" "$host_dir/databases/latest/" || return 1
  # shellcheck disable=SC2016
  "${run[@]}" <<<'rm -rf "$HOME/.cache/agent-thread-backup"' || true

  snapshot_dir="$host_dir/databases/$today"
  mkdir -p "$snapshot_dir"
  for f in "$host_dir"/databases/latest/*.sqlite; do
    [[ -e "$f" ]] || continue
    zstd -q -f -T0 -10 "$f" -o "$snapshot_dir/$(basename "$f").zst" || return 1
  done

  find "$host_dir/databases" -mindepth 1 -maxdepth 1 -type d \
    -name '????-??-??' -print | sort -r | tail -n "+$((keep_snapshots + 1))" |
    xargs -r rm -rf
}

failed=()
for spec in "$@"; do
  name="${spec%%=*}"
  target="${spec#*=}"
  echo "Backing up $name ($target)"
  if ! backup_host "$name" "$target"; then
    echo "Backup of $name failed" >&2
    failed+=("$name")
  fi
done

if ((${#failed[@]})); then
  echo "Failed hosts: ${failed[*]}" >&2
  exit 1
fi
