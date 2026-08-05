#!/usr/bin/env python3
"""Fetch a YouTube video's closed captions as plain text, with metadata.

Uses yt-dlp only to resolve metadata + caption track URLs (no video download),
then fetches the caption track directly. Prints a header block followed by the
transcript, either to stdout or to a file.
"""

from __future__ import annotations

import argparse
import json
import re
import subprocess
import sys
import urllib.request
from typing import Any

UA = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0 Safari/537.36"


def die(msg: str) -> "NoReturn":  # noqa: F821
    print(f"error: {msg}", file=sys.stderr)
    sys.exit(1)


def probe(url: str) -> dict[str, Any]:
    """Run yt-dlp in metadata-only mode."""
    cmd = ["yt-dlp", "-J", "--skip-download", "--no-warnings", url]
    try:
        proc = subprocess.run(cmd, capture_output=True, text=True, timeout=180)
    except FileNotFoundError:
        die("yt-dlp not found on PATH (try: nix shell nixpkgs#yt-dlp)")
    except subprocess.TimeoutExpired:
        die("yt-dlp timed out after 180s")
    if proc.returncode != 0:
        tail = (proc.stderr or "").strip().splitlines()[-5:]
        die("yt-dlp failed:\n  " + "\n  ".join(tail))
    try:
        return json.loads(proc.stdout)
    except json.JSONDecodeError:
        die("yt-dlp returned non-JSON output")


def rank_lang(code: str, want: str) -> int:
    """Lower is better. Exact match, then prefix match, then 'orig', then rest."""
    code = code.lower()
    want = want.lower()
    if code == want:
        return 0
    if code == f"{want}-orig":
        return 1
    if code.startswith(f"{want}-"):
        # Deprioritise machine-translated variants like en-de-DE.
        return 3 if code.count("-") > 1 else 2
    return 100


def pick_track(info: dict[str, Any], want: str, prefer_auto: bool) -> tuple[dict, str, bool]:
    """Choose one caption track. Returns (track, lang_code, is_auto)."""
    manual = info.get("subtitles") or {}
    auto = info.get("automatic_captions") or {}
    sources = [(auto, True), (manual, False)] if prefer_auto else [(manual, False), (auto, True)]

    best = None
    for table, is_auto in sources:
        for code, tracks in table.items():
            if code == "live_chat" or not tracks:
                continue
            score = rank_lang(code, want)
            if score >= 100:
                continue
            # Prefer json3, then srv3/vtt, then whatever is there.
            fmt_order = {"json3": 0, "srv3": 1, "vtt": 2, "srv1": 3}
            track = min(tracks, key=lambda t: fmt_order.get(t.get("ext", ""), 9))
            key = (0 if not is_auto else 1, score) if not prefer_auto else (0 if is_auto else 1, score)
            if best is None or key < best[0]:
                best = (key, track, code, is_auto)
        if best is not None:
            break

    if best is None:
        have = sorted(set(manual) | set(auto) - {"live_chat"})
        die(
            f"no '{want}' captions available. "
            + (f"available languages: {', '.join(have[:25])}" if have else "this video has no captions at all")
        )
    return best[1], best[2], best[3]


def fetch(url: str) -> bytes:
    req = urllib.request.Request(url, headers={"User-Agent": UA})
    with urllib.request.urlopen(req, timeout=60) as resp:
        return resp.read()


def parse_json3(raw: bytes) -> list[tuple[float, str]]:
    data = json.loads(raw)
    out = []
    for event in data.get("events", []):
        segs = event.get("segs")
        if not segs:
            continue
        text = "".join(s.get("utf8", "") for s in segs)
        text = text.replace("\n", " ").strip()
        if text:
            out.append((event.get("tStartMs", 0) / 1000.0, text))
    return out


def parse_vtt(raw: bytes) -> list[tuple[float, str]]:
    cues: list[tuple[float, str]] = []
    ts = re.compile(r"(\d+):(\d\d):(\d\d)[.,](\d\d\d)\s*-->")
    start = None
    buf: list[str] = []

    def flush():
        if start is not None and buf:
            text = re.sub(r"<[^>]+>", "", " ".join(buf)).strip()
            if text:
                cues.append((start, text))

    for line in raw.decode("utf-8", "replace").splitlines():
        m = ts.match(line.strip())
        if m:
            flush()
            buf = []
            h, mi, s, ms = (int(x) for x in m.groups())
            start = h * 3600 + mi * 60 + s + ms / 1000.0
        elif line.strip() and not line.startswith(("WEBVTT", "Kind:", "Language:", "NOTE")):
            buf.append(line.strip())
        elif not line.strip():
            flush()
            buf = []
            start = None
    flush()
    return cues


def dedupe(cues: list[tuple[float, str]]) -> list[tuple[float, str]]:
    """Auto-caption tracks repeat rolling context; drop lines contained in the previous one."""
    out: list[tuple[float, str]] = []
    for t, text in cues:
        if out:
            prev = out[-1][1]
            if text == prev or text in prev:
                continue
            if prev in text:
                out[-1] = (out[-1][0], text)
                continue
        out.append((t, text))
    return out


def hhmmss(sec: float) -> str:
    s = int(sec)
    h, rem = divmod(s, 3600)
    m, s = divmod(rem, 60)
    return f"{h}:{m:02d}:{s:02d}" if h else f"{m}:{s:02d}"


def chunk(cues: list[tuple[float, str]], every: float) -> list[str]:
    """Group cues into timestamped paragraphs."""
    if not cues:
        return []
    blocks: list[str] = []
    mark = cues[0][0]
    buf: list[str] = []
    for t, text in cues:
        if buf and t - mark >= every:
            blocks.append(f"[{hhmmss(mark)}] " + " ".join(buf))
            mark, buf = t, []
        buf.append(text)
    if buf:
        blocks.append(f"[{hhmmss(mark)}] " + " ".join(buf))
    return blocks


def main() -> None:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("url", help="YouTube URL or bare video ID")
    ap.add_argument("--lang", default="en", help="preferred caption language (default: en)")
    ap.add_argument("--auto", action="store_true", help="prefer auto-generated over human captions")
    ap.add_argument(
        "--interval",
        type=float,
        default=60.0,
        help="seconds per timestamped paragraph; 0 disables timestamps (default: 60)",
    )
    ap.add_argument("-o", "--out", help="write transcript to this file instead of stdout")
    ap.add_argument("--list-langs", action="store_true", help="list available caption languages and exit")
    args = ap.parse_args()

    url = args.url
    if re.fullmatch(r"[\w-]{11}", url):
        url = f"https://www.youtube.com/watch?v={url}"

    info = probe(url)

    if args.list_langs:
        manual = sorted(k for k in (info.get("subtitles") or {}) if k != "live_chat")
        auto = sorted(k for k in (info.get("automatic_captions") or {}) if k != "live_chat")
        print("human:", ", ".join(manual) or "(none)")
        print("auto: ", ", ".join(auto) or "(none)")
        return

    track, code, is_auto = pick_track(info, args.lang, args.auto)

    raw = fetch(track["url"])
    ext = track.get("ext", "")
    cues = parse_json3(raw) if ext == "json3" else parse_vtt(raw)
    if not cues:
        die(f"caption track '{code}' ({ext}) parsed to zero lines")
    if is_auto:
        cues = dedupe(cues)

    dur = info.get("duration") or 0
    header = [
        f"Title:    {info.get('title', '?')}",
        f"Channel:  {info.get('uploader', '?')}",
        f"Duration: {hhmmss(dur)}" if dur else "Duration: ?",
        f"Uploaded: {info.get('upload_date', '?')}",
        f"URL:      {info.get('webpage_url', url)}",
        f"Captions: {code} ({'auto-generated' if is_auto else 'human'})",
    ]
    body = chunk(cues, args.interval) if args.interval > 0 else [" ".join(t for _, t in cues)]
    doc = "\n".join(header) + "\n\n" + "\n\n".join(body) + "\n"

    if args.out:
        with open(args.out, "w") as fh:
            fh.write(doc)
        words = sum(len(b.split()) for b in body)
        print("\n".join(header))
        print(f"\nWrote {args.out} ({words} words, ~{words * 4 // 3} tokens, {len(body)} blocks)")
    else:
        sys.stdout.write(doc)


if __name__ == "__main__":
    main()
