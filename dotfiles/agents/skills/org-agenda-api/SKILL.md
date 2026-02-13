---
name: org-agenda-api
description: Use when interacting with the org-agenda-api HTTP server to read/write org-mode agenda data
---

# Org Agenda API Reference

HTTP API for org-mode agenda data. Use this skill when you need to query or modify org agenda entries programmatically.

## Authentication

Get credentials from pass:
```bash
pass show colonelpanic-org-agenda.fly.dev
```

Returns: password on first line, then `user:` and `url:` fields.

**Note:** The `url` field in pass may be outdated. Use the base URL below.

## Base URL

`https://colonelpanic-org-agenda.fly.dev`

All requests use Basic Auth with the credentials from pass.

## Read Endpoints

### GET /agenda
Get agenda entries for a day or week.

Query params:
- `span`: `day` (default) or `week`
- `date`: `YYYY-MM-DD` (default: today)
- `include_overdue`: `true` to include overdue items from previous days
- `include_completed`: `true` to include items completed on the queried date
- `refresh`: `true` to git pull repos first

Response includes `span`, `date`, `entries` array, and optionally `gitRefresh` results.

### GET /get-all-todos
Get all TODO items from agenda files.

Query params:
- `refresh`: `true` to git pull first

Response includes `defaults` (with `notifyBefore`), `todos` array, and optionally `gitRefresh`.

### GET /metadata
Get all app metadata in a single request. Returns:
- `templates`: capture templates
- `filterOptions`: tags, categories, priorities, todoStates
- `todoStates`: active and done states
- `customViews`: available custom agenda views
- `errors`: any errors encountered fetching above

### GET /todo-states
Get configured TODO states. Returns:
- `active`: array of not-done states (TODO, NEXT, etc.)
- `done`: array of done states (DONE, CANCELLED, etc.)

### GET /filter-options
Get available filter options. Returns:
- `todoStates`: all states
- `priorities`: available priorities (A, B, C)
- `tags`: all tags from agenda files
- `categories`: all categories

### GET /custom-views
List available custom agenda views. Returns array of `{key, name}` objects.

### GET /custom-view
Run a custom agenda view.

Query params:
- `key` (required): custom agenda command key
- `refresh`: `true` to git pull first

### GET /agenda-files
Get list of org-agenda-files with existence and readability status.

### GET /capture-templates (alias: /templates)
List available capture templates with their prompts.

### GET /health
Health check. Returns `status`, `uptime`, `requests`, and `captureStatus` if unhealthy.

### GET /version
Version info. Returns `version` and `gitCommit`.

### GET /debug-config
Current org configuration for debugging.

## Write Endpoints

### POST /capture
Create a new entry using a capture template.

**Important:** Use `capture-g` (GTD Todo) for most tasks - it properly records creation time and logbook history. Only use `default` when you specifically don't want GTD tracking.

Body:
```json
{
  "template": "capture-g",
  "values": {
    "Title": "Task title",
    "scheduled": "2026-01-20",
    "deadline": "2026-01-25",
    "priority": "A",
    "tags": ["work", "urgent"],
    "todo": "TODO"
  }
}
```

### POST /complete
Mark a TODO as complete.

Body (use any combination to identify the item):
```json
{
  "id": "org-id-if-available",
  "file": "/path/to/file.org",
  "pos": 12345,
  "title": "Task title",
  "state": "DONE"
}
```

Lookup order: id -> file+pos+title -> file+title -> title only

### POST /update
Update a TODO's scheduled date, deadline, priority, tags, or properties.

Body:
```json
{
  "id": "org-id",
  "file": "/path/to/file.org",
  "pos": 12345,
  "title": "Task title",
  "scheduled": "2026-01-20T10:00:00",
  "deadline": "2026-01-25",
  "priority": "B",
  "tags": ["updated", "tags"],
  "properties": {
    "CUSTOM_PROP": "value"
  }
}
```

Set value to `null` or empty string to clear. Response includes new `pos` for cache updates.

### POST /delete
Delete an org item permanently.

Body:
```json
{
  "id": "org-id",
  "file": "/path/to/file.org",
  "position": 12345,
  "include_children": true
}
```

Requires `include_children: true` if item has children, otherwise returns error.

### POST /restart
Restart the Emacs server (exits gracefully, supervisord restarts).

## Category Strategy Endpoints

These require org-category-capture to be configured.

### GET /category-types
List registered category strategy types. Returns array with:
- `name`: strategy type name
- `hasCategories`: boolean
- `captureTemplate`: template string
- `prompts`: array of prompt definitions

### GET /categories
Get categories for a strategy type.

Query params:
- `type` (required): strategy type name (e.g., "projects")
- `existing_only`: `true` to only return categories with capture locations

Returns `type`, `categories` array, `todoFiles` array.

### GET /category-tasks
Get tasks for a specific category.

Query params:
- `type` (required): strategy type name
- `category` (required): category name

### POST /category-capture
Capture a new entry to a category.

Body:
```json
{
  "type": "projects",
  "category": "my-project",
  "title": "Task title",
  "todo": "TODO",
  "scheduled": "2026-01-20",
  "deadline": "2026-01-25",
  "priority": "A",
  "tags": ["work"],
  "properties": {"EFFORT": "1h"}
}
```

## Response Format

Agenda/todo entries include:
- `todo`: TODO state (TODO, NEXT, DONE, etc.)
- `title`: Heading text
- `scheduled`: ISO date or datetime
- `deadline`: ISO date or datetime
- `priority`: A, B, or C (only if explicitly set)
- `tags`: Array of tags
- `file`: Source file path
- `pos`: Position in file (may change after edits)
- `id`: Org ID if set (stable identifier)
- `olpath`: Outline path array
- `level`: Heading level
- `category`: Category of the item
- `properties`: All properties from the property drawer
- `completedAt`: ISO timestamp when completed (if applicable)
- `agendaLine`: Raw agenda display text (agenda endpoint only)
- `notifyBefore`: Array of minutes for notifications
- `isWindowHabit`: Boolean for window habits
- `habitSummary`: Summary object for habits (if applicable)

## Common Workflows

**View today's agenda:**
```bash
curl -s -u "$USER:$PASS" "$URL/agenda?span=day" | jq '.entries[] | {todo, title, scheduled}'
```

**View this week:**
```bash
curl -s -u "$USER:$PASS" "$URL/agenda?span=week" | jq .
```

**View completed tasks for a specific date:**
```bash
curl -s -u "$USER:$PASS" "$URL/agenda?date=2026-01-17&include_completed=true" | jq '.entries[] | select(.completedAt != null) | {title, completedAt}'
```

**Get all metadata at once:**
```bash
curl -s -u "$USER:$PASS" "$URL/metadata" | jq .
```

**Create a task:**
```bash
curl -s -u "$USER:$PASS" -X POST "$URL/capture" \
  -H "Content-Type: application/json" \
  -d '{"template":"capture-g","values":{"Title":"New task","scheduled":"2026-01-20"}}'
```

**Complete a task by title:**
```bash
curl -s -u "$USER:$PASS" -X POST "$URL/complete" \
  -H "Content-Type: application/json" \
  -d '{"title":"Task title"}'
```

**Update a task's schedule:**
```bash
curl -s -u "$USER:$PASS" -X POST "$URL/update" \
  -H "Content-Type: application/json" \
  -d '{"title":"Task title","scheduled":"2026-01-21T14:00:00"}'
```

**Clear a deadline:**
```bash
curl -s -u "$USER:$PASS" -X POST "$URL/update" \
  -H "Content-Type: application/json" \
  -d '{"title":"Task title","deadline":null}'
```

**Delete a task:**
```bash
curl -s -u "$USER:$PASS" -X POST "$URL/delete" \
  -H "Content-Type: application/json" \
  -d '{"title":"Task to delete","file":"/path/to/file.org","position":12345}'
```

## Error Handling

All endpoints return JSON. Errors include:
```json
{
  "status": "error",
  "message": "Error description"
}
```

Success responses include:
```json
{
  "status": "created" | "completed" | "updated",
  ...additional fields
}
```
