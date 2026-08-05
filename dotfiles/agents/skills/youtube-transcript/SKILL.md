---
name: youtube-transcript
description: Summarize or answer questions about a YouTube video by pulling its closed captions instead of watching it. Use when given a YouTube URL or video ID and asked what it's about, for a summary, for key points or timestamps, for a specific quote or claim, or to check whether a video covers some topic.
---

# YouTube video summaries from closed captions

Nearly every YouTube video ships captions — human-authored or auto-generated. Fetching
them costs a couple of seconds and gives you the full content of the video as text. Do
that instead of guessing from the title, and never claim you watched a video.

## Get the transcript

```bash
scripts/yt-transcript.py <url-or-id> -o /tmp/yt-<id>.txt
```

Paths are relative to this skill's directory. The script writes a header block (title,
channel, duration, upload date, caption source) plus the timestamped transcript to the
file, and prints the header and a word/token count to stdout. Read the file afterward.

Send output to a file rather than stdout for anything longer than a few minutes — a
one-hour talk is roughly 10k words, and you want it as one deliberate read.

Useful flags:

| Flag | Purpose |
| --- | --- |
| `--interval N` | Seconds per timestamped paragraph (default 60). Use `120`–`300` for long videos, `30` when you need precise timestamps, `0` for one unbroken block. |
| `--lang CODE` | Preferred caption language (default `en`). |
| `--auto` | Prefer auto-generated captions over human ones (occasionally cleaner on channels with bad fan subs). |
| `--list-langs` | Show available caption languages and exit. |

The script uses `yt-dlp` for metadata and caption-track URLs only — it never downloads
video or audio. If `yt-dlp` is missing from `PATH`, prefix the command with
`nix shell nixpkgs#yt-dlp --command`.

## Then summarize

Lead with what the video actually argues or shows, not a description of its structure.
A useful default shape:

1. **One or two sentences** on what the video is and its central claim or purpose.
2. **Key points** as a short list, each with a `[m:ss]` timestamp from the transcript so
   the user can jump to it.
3. **Anything notable** — a surprising claim, a caveat the speaker gives, a conclusion
   that differs from the title's implication.

Match length to the ask. "What's this about?" wants three sentences; "summarize this
talk" wants the full shape above. If asked a specific question, answer it directly and
quote the relevant line with its timestamp rather than summarizing the whole video.

Quote sparingly — short excerpts to support a point. Do not reproduce long stretches of
the transcript verbatim, and never reproduce song lyrics; summarize instead.

## Caption quality caveats

Auto-generated captions have no speaker labels, no punctuation reliability, and
mistranscribe proper nouns and jargon. The header line tells you which kind you got.

- With auto captions, treat unusual names, numbers, and technical terms as suspect.
  Say "the speaker refers to what the captions render as X" when it matters.
- Captions carry no visual information. If the video is a demo, chart walkthrough, or
  anything where the point is on screen, the transcript will have gaps like "as you can
  see here" — flag that limitation rather than filling it in.
- Music, applause, and effects appear as `[Music]`-style markers.

## When there are no captions

The script exits with an error listing available languages, or says the video has none.
Options, in order:

1. Retry with `--list-langs`; the content may exist under a different code, or only as
   auto-generated (which the default already prefers second).
2. If only a non-English track exists, fetch it with `--lang` and summarize in English.
3. If there are genuinely no captions, say so. Do not download and transcribe the audio
   unless the user explicitly asks — that is a much heavier operation.

Live streams in progress, age-restricted videos, and members-only content may fail at
the metadata step. Report the error rather than working around it.
