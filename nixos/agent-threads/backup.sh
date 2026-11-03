# Usage: agent-thread-backup NAME=SSH_TARGET...
#
# Pulls T3 Code, Paseo, Claude Code, Codex, and opencode thread history from
# each host into $AGENT_THREAD_BACKUP_DIR/NAME, then rebuilds index.sqlite.
# Transcripts are mirrored without --delete, so files a host prunes stay in
# the backup. SQLite stores are snapshotted on the host, then kept as dated
# zstd copies.

dest_root="${AGENT_THREAD_BACKUP_DIR:-$HOME/Backups/agent-threads}"
keep_snapshots="${AGENT_THREAD_BACKUP_KEEP:-14}"
local_host="$(uname -n)"
local_host="${local_host%%.*}"
today="$(date +%F)"

# Runs on each host under its own bash (macOS ships 3.2). Prints what to
# copy, one "d DIR", "f FILE", or "s SNAPSHOT" per line, relative to $HOME.
read -r -d '' remote_prepare <<'EOF' || true
cd "$HOME" || exit 1
stage=.cache/agent-thread-backup
rm -rf "$stage"
snapshot() {
  [ -f "$1" ] || return 0
  if python3 -I -c '
import sqlite3, sys
src = sqlite3.connect(sys.argv[1], timeout=60)
dst = sqlite3.connect(sys.argv[2])
src.backup(dst)
dst.close()
src.close()
' "$1" "$stage/$2.part" && mv "$stage/$2.part" "$stage/$2"; then
    printf 's %s\n' "$2"
  else
    rm -f "$stage/$2.part"
    echo "snapshot of $1 failed" >&2
  fi
}
if mkdir -p "$stage"; then
  snapshot .t3/userdata/statev2.sqlite t3-statev2.sqlite
  snapshot .local/share/opencode/opencode-stable.db opencode.sqlite
fi
for d in .claude*/projects .codex*/sessions .codex*/archived_sessions \
  .paseo/agents .paseo/projects .paseo/schedules .paseo/loops \
  .paseo/creations .paseo/agent-requests; do
  [ -d "$d" ] && printf 'd %s\n' "$d"
done
for f in .claude*/history.jsonl .codex*/history.jsonl; do
  [ -f "$f" ] && printf 'f %s\n' "$f"
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
  local src listing kind path f snapshot_dir status=0 snapshots=0

  if [[ "$name" == "$local_host" ]]; then
    run=(bash -s)
    src="$HOME/"
  else
    run=(ssh "${ssh_opts[@]}" "$target" bash -s)
    src="$target:"
  fi

  listing="$("${run[@]}" <<<"$remote_prepare")" || return 1

  while read -r kind path; do
    case "$kind" in
      d) rsync -a --mkpath -e "ssh ${ssh_opts[*]}" "$src$path/" "$host_dir/home/$path/" || status=1 ;;
      f) rsync -a --mkpath -e "ssh ${ssh_opts[*]}" "$src$path" "$host_dir/home/$path" || status=1 ;;
      s) snapshots=1 ;;
    esac
  done <<<"$listing"

  if ((snapshots)); then
    rsync -a --mkpath -e "ssh ${ssh_opts[*]}" --include='*.sqlite' --exclude='*' \
      "$src.cache/agent-thread-backup/" "$host_dir/databases/latest/" || status=1
  fi
  # shellcheck disable=SC2016
  "${run[@]}" <<<'rm -rf "$HOME/.cache/agent-thread-backup"' || true

  if ((snapshots)); then
    snapshot_dir="$host_dir/databases/$today"
    mkdir -p "$snapshot_dir"
    while read -r kind path; do
      [[ "$kind" == s ]] || continue
      f="$host_dir/databases/latest/$path"
      zstd -q -f -T0 -10 "$f" -o "$snapshot_dir/$path.zst" || status=1
    done <<<"$listing"
    find "$host_dir/databases" -mindepth 1 -maxdepth 1 -type d \
      -name '????-??-??' -print | sort -r | tail -n "+$((keep_snapshots + 1))" |
      xargs -r rm -rf
  fi
  return "$status"
}

failed=()
reached=()
for spec in "$@"; do
  name="${spec%%=*}"
  target="${spec#*=}"
  echo "Backing up $name ($target)"
  if backup_host "$name" "$target"; then
    reached+=("$spec")
  else
    echo "Backup of $name failed" >&2
    failed+=("$name")
  fi
done

echo "Indexing"
agent-threads-index build --root "$dest_root" || failed+=(index)
for spec in "${reached[@]}"; do
  agent-threads-index resolve-repos "${spec%%=*}" "${spec#*=}" --root "$dest_root" || true
done
agent-threads-index repos --root "$dest_root" || failed+=(index)

if ((${#failed[@]})); then
  echo "Failed: ${failed[*]}" >&2
  exit 1
fi
