---
name: org-agenda-api-production
description: Use when investigating production org-agenda-api state, testing endpoints, or debugging production issues
---

# org-agenda-api Production Access

## Overview

Access the production org-agenda-api instance at https://colonelpanic-org-agenda.fly.dev/ for debugging, testing, or verification.

## Credentials

Get the password from `pass`:
```bash
pass show org-agenda-api/imalison
```

Username is currently `imalison`.

## Quick Access with just

This repo includes a `justfile` under `~/dotfiles/org-agenda-api` with pre-configured commands:

```bash
cd ~/dotfiles/org-agenda-api
just health
just get-all-todos
just get-todays-agenda
just agenda
just agenda-files
just todo-states
just create-todo "Test todo"
```

## Manual curl

Prefer using the `just` recipes above so we don't bake auth syntax into docs.

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
