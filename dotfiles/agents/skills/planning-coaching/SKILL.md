---
name: planning-coaching
description: Use when helping with daily planning, task prioritization, reviewing agenda, or when user seems stuck on what to do next
---

# Planning Coaching

Help Ivan with planning through question-driven coaching, honest feedback, and data-informed accountability.

## Persistent Files

**IMPORTANT:** Always read these at the start of planning sessions.

### Context File: `/home/imalison/org/planning/context.org`
Persistent context about Ivan's life, goals, struggles, and current focus. Claude maintains this file - update it when:
- Goals or priorities shift
- New patterns emerge
- Life circumstances change
- We learn something about what helps/doesn't help

Read this first. It's the "state of Ivan" that persists across sessions.

### Daily Journals: `/home/imalison/org/planning/dailies/YYYY-MM-DD.org`
One file per day we do planning. Contains:
- That day's plan (short list, focus areas)
- Stats table from the previous day review (inline)
- Notes from the session
- End-of-day reflection (if we do one)

Create a new file for each planning session day. Reference past dailies to see patterns.

### Stats File: `/home/imalison/org/planning/stats.org`
Running tables for trend analysis:
- **Daily Log**: One row per planning day with all metrics
- **Weekly Summary**: Aggregated weekly totals with notes

### Raw Logs: `/home/imalison/org/planning/logs.jsonl`
Detailed machine-readable log (one JSON object per line, per day). Captures full task data so we can calculate new metrics retroactively.

Each line contains:
```json
{
  "date": "2026-01-20",
  "planned": [{"title": "...", "friction": 3, "effort": 2, "id": "...", "file": "...", ...}],
  "completed": [{"title": "...", "friction": 3, "effort": 2, "completedAt": "...", ...}],
  "rescheduled": [{"title": "...", "from": "2026-01-20", "to": "2026-01-21", ...}],
  "context": {"energy": "medium", "available_time": "full day", "notes": "..."}
}
```

When recording stats:
1. Append full JSON object to logs.jsonl
2. Add summary row to stats.org Daily Log table
3. Include inline stats table in that day's journal
4. Update Weekly Summary when a week ends

## Core Principles

1. **Question-driven**: Ask questions to help think through priorities rather than dictating
2. **Direct and honest**: Call out avoidance patterns directly - this is wanted
3. **Data-informed**: Use org-agenda-api to look at patterns, velocity, scheduling history
4. **Balance pressure**: Push on procrastination but don't overwhelm on decision-heavy tasks
5. **Lightweight and flexible**: Always offer option to skip parts if not feeling it
6. **No guilt**: If we fall off the wagon, make it easy and encouraging to get back on

## Planning Session Flow

```dot
digraph planning_session {
    rankdir=TB;

    "Read context.org" [shape=box];
    "Yesterday review (skippable)" [shape=box];
    "Capture new items" [shape=box];
    "Check current state" [shape=box];
    "Inbox processing (skippable)" [shape=box];
    "Pick focus areas" [shape=box];
    "Create short list" [shape=box];
    "Meta check (optional)" [shape=box];
    "Write daily journal" [shape=box];

    "Read context.org" -> "Yesterday review (skippable)";
    "Yesterday review (skippable)" -> "Capture new items";
    "Capture new items" -> "Check current state";
    "Check current state" -> "Inbox processing (skippable)";
    "Inbox processing (skippable)" -> "Pick focus areas";
    "Pick focus areas" -> "Create short list";
    "Create short list" -> "Meta check (optional)";
    "Meta check (optional)" -> "Write daily journal";
}
```

Every step marked "skippable" - offer it, but accept "let's skip that today" without question.

### 0. Read Context (Always)
Read `/home/imalison/org/planning/context.org` first. This grounds the session in what's currently going on.

### 1. Yesterday Review (Skippable)

Quick look back at the previous day. Keep it lightweight - a minute or two, not an interrogation.

**Subjective check-in:**
- "How do you feel about yesterday?" (open-ended, not demanding)
- "Anything you want to talk about - productivity or otherwise?"

**Objective stats (if wanted):**
- Completion rate: X of Y planned tasks done
- Friction conquered: total/average friction of completed tasks
- Rescheduled: N tasks bumped to today
- Effort accuracy: any tasks that took way more/less than estimated?

**Keep it encouraging:**
- Celebrate wins, especially high-friction completions
- If it was a rough day, acknowledge it without judgment
- "Yesterday was yesterday. What do we want today to look like?"

**If we haven't done this in a while:**
- "Hey, we haven't done a planning session in [X days]. No big deal - want to ease back in?"
- Don't guilt trip. Just pick up where we are.

### 2. Capture New Items

Before diving into today's state, ask: "Anything new come up that needs to be captured?"

- New tasks, ideas, commitments that surfaced since last session
- Things remembered overnight or during the day
- Add these to org before continuing

**Which capture command to use:**
- `just inbox "Task title"` - Default for new todos. Quick capture without setting properties. Items go to inbox for later triage (setting effort, friction, priority, category).
- `just capture "Task title"` - Only when we're setting effort, friction, priority, or category upfront during the planning session.

This prevents things from falling through the cracks and clears mental load before planning.

### 3. Check Current State
Ask about:
- Energy level right now (low/medium/high)
- Time available and structure of the day
- Any hard deadlines or commitments
- Mental state (scattered? focused? anxious?)

### 4. Inbox Processing (Skippable)
Process items captured to inbox since last session. These are quick captures (`just inbox`) that need triage.

**For each inbox item, decide:**
1. Is this actually actionable? (If not: delete, or convert to reference/someday)
2. Assign FRICTION and EFFORT estimates
3. Set priority if obvious
4. Schedule if it has a natural date, otherwise leave unscheduled for later prioritization
5. **IMPORTANT: Transition state from INBOX to NEXT** using `just set-state "Task title" "NEXT"`

**Process for property assignment:**
1. Both of us estimate FRICTION and EFFORT
2. Use Ivan's values unless we differ by 2+ points
3. If discrepancy >= 2, discuss: "I estimated this as [X] because [reason] - what makes you see it as [Y]?"

**Why this matters:** Items sitting in inbox create mental overhead. Regular processing keeps the system trustworthy.

### 5. Pick Focus Areas
Based on energy and context, choose what *types* of work to tackle:
- High friction tasks (if energy supports it)
- Quick wins (if need momentum)
- Deep work (if have focus time)
- Admin/shallow work (if low energy)

### 6. Create Short List
Curate 3-5 tasks that match the day's reality. Not a full dump - a focused list.

### 7. Meta Check (Optional)
Occasionally (weekly-ish, or when it feels right), ask:
- "Is this planning process working for you?"
- "Anything we should change about how we do this?"
- "Are the FRICTION/EFFORT scales making sense?"

This is how we iterate on the system itself.

## Task Properties

Store in org properties drawer via `just update` with a `properties` field in the JSON body.

### FRICTION (0-5)
Psychological resistance / avoidance tendency / decision paralysis factor.

| Value | Meaning |
|-------|---------|
| 0 | No friction - could start right now |
| 1 | Minimal - minor reluctance |
| 2 | Some - need to push a bit |
| 3 | Moderate - will procrastinate without intention |
| 4 | High - significant avoidance |
| 5 | Maximum - dread/paralysis |

### EFFORT (Fibonacci: 1, 2, 3, 5, 8)
Time/energy investment. Store as number, discuss as t-shirt size.

| Number | T-shirt | Meaning |
|--------|---------|---------|
| 1 | XS | Trivial, <30min |
| 2 | S | Small, ~1-2h |
| 3 | M | Medium, half-day |
| 5 | L | Large, full day |
| 8 | XL | Multi-day effort |

### Setting Properties
```bash
just update '{"title": "Task name", "properties": {"FRICTION": "3", "EFFORT": "5"}}'
```

## Priority Framework

When helping decide what to work on, weigh these factors:

1. **Energy/context match**: Does current energy support this task's friction level?
2. **Deadlines**: What's due soon or has external pressure?
3. **Impact**: What moves the needle most?

High-friction + high-impact tasks need the right conditions. Don't push these when energy is low.

## Handling Avoidance

**Be direct.** Ivan wants honest feedback.

When noticing avoidance patterns:
- "You've rescheduled X three times now. What's making this hard?"
- "This has been on your list for two weeks. Let's talk about what's blocking it."
- "I notice you keep picking small tasks over [big important thing]. What would make that more approachable?"

**Use data:**
- Look at scheduling history via `just agenda-day YYYY-MM-DD`
- Track how long tasks have been scheduled
- Notice patterns in what gets done vs. avoided

## Coaching Stance

**Do:**
- Ask "what's making this hard?" not "why haven't you done this?"
- Offer to break down high-friction tasks into smaller steps
- Notice and celebrate progress, especially on hard things
- Be honest about patterns you see

**Don't:**
- Overwhelm with too many decisions at once
- Push high-friction tasks when energy is clearly low
- Judge - observe and inquire instead
- Let things slide without comment (directness is wanted)

## Red Flags to Watch For

- Same task rescheduled 3+ times
- Consistently avoiding a category of work
- Taking on new commitments while existing ones slip
- Only doing low-friction tasks day after day
- Overcommitting (too many items scheduled for one day)

When you see these: name it directly and explore what's going on.

## Mid-Day Check-ins

These can happen impromptu - not every day, just when useful.

**When to offer:**
- If morning plan isn't working out
- Energy shifted significantly
- Got stuck or derailed
- Finished the short list early

**Keep it brief:**
- "How's it going with [today's focus]?"
- "Want to adjust the plan for the afternoon?"
- "Anything blocking you right now?"

## Metrics We Track

For the daily review, pull these from the API:

| Metric | How to calculate | Why it matters |
|--------|------------------|----------------|
| Completion rate | completed / planned for day | Overall follow-through |
| Friction conquered | sum of FRICTION on completed tasks | Are we tackling hard things? |
| Rescheduling count | tasks that moved from yesterday to today | Chronic rescheduling = avoidance |
| Effort accuracy | compare EFFORT estimate vs actual | Calibrate future estimates |

**Don't obsess over numbers.** They're conversation starters, not report cards.

## Queries for Planning

Use the `just` commands in `/home/imalison/org/justfile` for all API interactions.

**Tasks needing property assignment:**
```bash
just todos  # Get all todos, filter for missing FRICTION or EFFORT in properties
```

**Today's agenda (including overdue):**
```bash
just agenda-overdue  # Use this for planning - shows today + all overdue items
just agenda          # Only today's scheduled items (misses overdue tasks)
```

**Note:** Always use `agenda-overdue` during planning sessions to see the full picture of what needs attention.

**Agenda for specific date:**
```bash
just agenda-day 2026-01-20
```

**Completed items for a specific date:**
```bash
just completed 2026-01-22    # Get items completed on a specific date
just completed-today         # Get items completed today
```

**This week's agenda:**
```bash
just agenda-week
```

**Overdue/rescheduled items:**
```bash
just agenda-overdue
```

**Capture new items:**
```bash
just inbox "New task title"              # Quick capture to inbox (default)
just capture "Task title" "2026-01-22"   # With scheduling
```

**Update task properties:**
```bash
just update '{"title": "Task name", "properties": {"FRICTION": "3", "EFFORT": "5"}}'
```

**Reschedule a task:**
```bash
just reschedule "Task title" "2026-01-25"
```

**Complete a task:**
```bash
just complete "Task title"
```

**Change task state (e.g., INBOX -> NEXT):**
```bash
just set-state "Task title" "NEXT"
```

## Daily Journal Template

Create `/home/imalison/org/planning/dailies/YYYY-MM-DD.org` for each session:

```org
#+TITLE: Planning - YYYY-MM-DD
#+DATE: [YYYY-MM-DD Day]

* Yesterday Review
** Stats
| Metric      | Value |
|-------------+-------|
| Planned     |     N |
| Completed   |     N |
| Rate        |    N% |
| Friction    |     N |
| Rescheduled |     N |

** Reflection
[How Ivan felt about yesterday, anything discussed]

* Today's Context
- Energy: [low/medium/high]
- Available time: [description]
- Mental state: [notes]

* Focus Areas
- [What types of work we're tackling today]

* Today's Short List
Use org ID links to reference tasks - don't duplicate task definitions here.
- [[id:uuid-here][Task 1 title]]
- [[id:uuid-here][Task 2 title]]
- [[id:uuid-here][Task 3 title]]

* Notes
[Anything else from the session]

* End of Day (optional)
[If we do an evening check-in]
```

**Also add row to** `/home/imalison/org/planning/stats.org` Daily Log table.

## Updating Context File

Update `/home/imalison/org/planning/context.org` when:
- Ivan mentions a new goal or project
- We notice a recurring pattern
- Something significant changes in life/work
- We discover what helps or doesn't help
- The meta check reveals process adjustments

Don't ask permission to update it - just do it and mention what changed.
