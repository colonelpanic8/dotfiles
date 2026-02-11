---
name: org-agenda-api-production
description: Use when investigating production org-agenda-api state, testing endpoints, or debugging production issues
---

# org-agenda-api Production Access

## Overview

Access the production org-agenda-api instance at https://colonelpanic-org-agenda.fly.dev/ for debugging, testing, or verification.

## Credentials

Get credentials from pass:
```bash
pass show colonelpanic-org-agenda.fly.dev
```

Format:
- Line 1: password
- `user:` field: username

## Quick Access with just

The project includes a justfile with pre-configured commands:

```bash
# Authenticated curl to any endpoint
just prod /health
just prod /get-all-todos
just prod "/agenda?span=week"

# Common shortcuts
just health      # Health check
just version     # API version
just todos       # All todos
just today       # Today's agenda
just agenda      # Day agenda (or: just agenda week)
just templates   # Capture templates
just metadata    # Full metadata
```

## Manual curl

```bash
curl -s -u "$(pass show colonelpanic-org-agenda.fly.dev | grep '^user:' | cut -d' ' -f2):$(pass show colonelpanic-org-agenda.fly.dev | head -1)" \
  https://colonelpanic-org-agenda.fly.dev/health | jq
```

## Key Endpoints

| Endpoint | Method | Description |
|----------|--------|-------------|
| /health | GET | Health check |
| /version | GET | API version |
| /get-all-todos | GET | All TODO items |
| /agenda | GET | Agenda (span=day\|week) |
| /capture | POST | Create entry |
| /update | POST | Update heading |
| /complete | POST | Complete item |
| /delete | POST | Delete heading |
