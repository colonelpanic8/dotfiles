---
name: password-reset
description: Use when the user wants to reset or rotate a website or service password end-to-end, including finding the right `pass` entry, generating a new password with `xkcdpassgen`, retrieving reset emails through `gws gmail` or a local mail CLI, completing the reset in the browser with Chrome DevTools MCP, and updating the password store safely without losing entry metadata.
---

# Password Reset

## Overview

Handle password resets end-to-end. Prefer `gws gmail` for reset-email retrieval, Chrome DevTools MCP for website interaction, and the local `xkcdpassgen` helper for password generation.

## Tool Priorities

- Prefer `gws gmail` over opening Gmail in the browser.
- If `gws` is unavailable, use an installed Gmail CLI or IMAP-based mail tool if one exists locally. Inspect the environment first instead of guessing command names.
- Prefer Chrome DevTools MCP for all browser interaction.
- Use `pass find` and `pass show` before asking the user for credentials or account details.

## Password Generation

The local password generator is `xkcdpassgen`, defined in `dotfiles/lib/functions/xkcdpassgen` and available in shell as an autoloaded function.

```bash
xkcdpassgen <pass-entry-name>
```

Behavior:

- Generates `xkcdpass -n 3 | tr -d ' '` as the base password.
- Appends one uppercase letter, one digit, and one symbol by default.
- Supports:
  - `-U` to omit uppercase
  - `-N` to omit number
  - `-S` to omit symbol

	Do not substitute a different password generator ungless the user explicitly asks.

## Safe `pass` Update Pattern

`xkcdpassgen` writes directly to the `pass` entry it is given. Do not run it against the canonical entry before the reset succeeds, because:

- it would overwrite the current password immediately
- it would replace any extra metadata lines in a multiline `pass` entry

Use this pattern instead:

```bash
entry="service/example"
tmp_entry="${entry}-password-reset-tmp"

existing_contents="$(pass show "$entry" 2>/dev/null || true)"
metadata="$(printf '%s\n' "$existing_contents" | tail -n +2)"

xkcdpassgen "$tmp_entry"
new_password="$(pass show "$tmp_entry" | head -1)"

# ... use $new_password in the reset flow ...

if [ -n "$metadata" ]; then
  printf '%s\n%s\n' "$new_password" "$metadata" | pass insert -m -f "$entry"
else
  printf '%s\n' "$new_password" | pass insert -m -f "$entry"
fi

pass rm -f "$tmp_entry"
```

If the site rejects the password because of policy constraints, keep the canonical entry unchanged, delete or reuse the temp entry, and generate another candidate with different flags only if needed.

## Reset Workflow

1. Identify the account and canonical `pass` entry.
2. Run `pass find <service>` and inspect likely matches with `pass show`.
3. Capture existing metadata before generating a new password.
4. Generate the candidate password into a temporary `pass` entry with `xkcdpassgen`.
5. Start the reset flow in Chrome DevTools MCP:
   - navigate to the login or account page
   - use the site's "forgot password" flow, or
   - sign in and navigate to security settings if the user asked for a rotation rather than a reset
6. Use `gws gmail` to retrieve the reset email when needed:
   - search recent mail by sender domain, subject, or reset-related keywords
   - open the message and extract the reset link
   - navigate to that link in Chrome DevTools MCP
7. Fill the new password from the temporary `pass` entry and complete the form.
8. Verify success:
   - confirmation page, or
   - successful login with the new password
9. Promote the temp password into the canonical `pass` entry while preserving metadata, then remove the temp entry.

## Email Guidance

Prefer `gws gmail` for reset-email handling. Typical pattern:

- list recent messages with `gws gmail users messages list --params '{"userId":"me","q":"from:service.example newer_than:7d"}'`
- bias toward reset keywords such as `reset`, `password`, `security`, `verify`, or `signin`
- read shortlisted messages with `gws gmail users messages get --params '{"userId":"me","id":"MESSAGE_ID","format":"full"}'` rather than browsing Gmail manually

If `gws` is unavailable, use an installed Gmail CLI or local mail helper only as a fallback. Keep that discovery lightweight and local to the current environment.

## Browser Guidance

Use Chrome DevTools MCP to complete the reset flow directly:

- navigate to the reset or security page
- take snapshots to identify the relevant inputs and buttons
- click, fill, and submit through the site UI
- verify the success state before updating the canonical `pass` entry

Prefer MCP interaction over describing steps for the user to perform manually.

## Credentials And Account Data

- Search `pass` before asking the user for usernames, recovery emails, or OTP-related entries.
- Preserve existing metadata lines in multiline `pass` entries whenever possible.
- Never print the new password in the final response unless the user explicitly asks for it.

## Failure Handling

- If account discovery is ambiguous, ask a short clarifying question only after checking `pass`.
- If the reset email does not arrive, search spam or alternate senders before giving up.
- If login or reset requires another secret that is not in `pass`, then ask the user.
- If the reset flow fails after temp-password generation, leave the canonical entry untouched.
