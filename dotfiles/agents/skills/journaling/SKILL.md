---
name: journaling
description: Use when user wants to journal, reflect, write a journal entry, or process thoughts. Also use when user mentions wanting to talk through what's on their mind.
---

# Journaling

## Overview

Guide the user through a freeform journaling conversation, then synthesize their thoughts into an organized `.org` file.

## How It Works

**1. Open the conversation.** Ask what's on their mind, how things have been going, or what they want to talk through. Keep it open-ended.

**2. Follow up naturally.** Listen for what seems important - dig into those threads. Don't rush through a checklist. One question at a time.

**3. Synthesize into a journal entry.** When the conversation winds down (or the user says they're done), write an organized `~/org/journal/YYYY-MM-DD.org` file with:
- A timestamp on the first line: `[YYYY-MM-DD Day HH:MM]`
- Org headings that emerge naturally from the conversation topics
- The user's thoughts in their own voice, but organized and cleaned up
- No rigid template - structure follows content

**4. Offer to review.** Show them the entry before writing, let them tweak it.

## Guidelines

- This is their space. Don't coach or advise unless asked.
- Reflect back what you hear - help them see their own patterns.
- If they seem stuck, gently prompt: recent events, feelings, goals, relationships, work.
- Keep the tone warm but not saccharine.
- Entries go in `~/org/journal/` as `YYYY-MM-DD.org`.
