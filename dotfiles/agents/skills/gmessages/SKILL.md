---
name: gmessages
description: Use the gmessages CLI to find conversations, read or search Ivan's SMS and RCS texts, wait for verification codes, or check the Google Messages bridge. Also covers the separate WhatsApp profile.
---

# Google Messages CLI

`gmessages` is preconfigured on Ivan's NixOS hosts. The default profile reads SMS and RCS through the bridge on `jimi-hendnix` over Tailscale. Run `gmessages status` to check the connection and phone, and `gmessages help` for the command list.

## Find and read texts

```sh
gmessages conversations --query 'Sender name' --limit 5
gmessages messages --limit 10 'Conversation ID'
gmessages search --since 15m --direction incoming --limit 10
gmessages --json search --query 'keywords' --conversation 'Conversation ID' --since 24h
```

Use a conversation ID, phone number, or part of a name. Use the ID from `conversations` when a name is ambiguous. `messages` shows the latest page in chronological order and accepts `--before` with the returned `next_before` cursor for older messages. `search` returns newest matches first. Filter by sender, query, or time window to keep reads relevant to the request.

## Wait for an incoming text

```sh
gmessages wait --conversation 'Sender name' --direction incoming --since 5m --timeout 3m
```

Use `--since` to include texts that may have arrived before `wait` started. Omit it to watch for new arrivals. Add `--query` when you know part of the message. Exit status 2 means no matching text arrived before the timeout. For verification codes, use the matching text from the current attempt and keep the code out of user-facing summaries.

## Photos and other attachments

```sh
gmessages search --conversation 'Sender name' --attachments --limit 3
gmessages attachment 'ATTACHMENT_ID'            # prints the saved path and type
gmessages attachment 'ATTACHMENT_ID' -o ./dir   # or a file path; -o - for stdout
gmessages attachment 'ATTACHMENT_ID' --request  # thumbnail-only: ask the phone for the original
```

Messages list attachments as `[attachment NAME TYPE SIZE id=ID]`. `attachment` saves to `$XDG_RUNTIME_DIR/gmessages/attachments` by default; open the printed path with your image/file reader. The type comes from the bytes, so a `.heic` name often downloads as JPEG. An attachment flagged `thumbnail-only full-requestable` downloads a small preview; after `--request`, re-read the message a few seconds later, because the original arrives under a new attachment ID. Over MCP, `get_attachment` returns the saved path and shows photos inline.

## Profiles and access

- `gmessages profiles` lists configured profiles. For WhatsApp, run `gmessages --profile whatsapp status` or other commands with that profile on `jimi-hendnix`.
- If `status` cannot reach the bridge, check Tailscale connectivity to `jimi-hendnix`. If the bridge responds, use its connection and phone status to diagnose the issue. The managed CLI configuration uses a token file. Keep its contents out of logs and chat.
- The configured `gmessages` MCP server provides read-only access when needed. Sending a text through `gmessages send` requires an explicit user request.
