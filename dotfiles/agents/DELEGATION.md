# Delegation, Model, and Effort Selection

## Delegation
- As Fable 5, when the work is substantial and pieces of it wouldn't benefit
  from Fable-level intelligence, lean toward orchestrating — delegate those
  pieces to subagents rather than doing everything directly. Other primaries:
  use judgment.
- The orchestrator is responsible for reviewing and integrating delegated work.
- Record a one-line justification for each model-tier choice, tied to what the
  agent must actually decide at execution time. If you can't articulate why the
  cheaper tier is insufficient, use it.

## Models

Subjective routing scores, 1–10 (cost: 10 = most expensive).

| Model        | Intelligence / judgment | Design sense | Steerability | Detail orientation | Cost |
| ------------ | ----------------------: | -----------: | -----------: | -----------------: | ---: |
| Fable 5      |                      10 |           10 |            7 |                  9 |   10 |
| GPT-5.6 Sol  |                       9 |          8.5 |           10 |                9.5 |    6 |
| Opus 5       |                       8 |          9.5 |            6 |                  8 |    7 |
| GPT-5.6 Luna |                     6.5 |            5 |            6 |                6.5 |    1 |

- **Sol** — default workhorse for implementation and long-running agentic
  work, especially anything that should run to completion unattended. Best for
  detail-oriented work and catching bugs: debugging, careful review, grinding
  a stated goal to completion.
- **Opus 5** — near peer of Sol; choose on working style. Prefer it where
  design and code are entangled (UI, API surfaces, naming-heavy refactors), and
  where the value is judgment rather than execution: investigation, design
  critique, second opinions.
- **Fable 5** — design planning, architecture, and work where ambiguity or
  taste materially affects the result; also genuinely tough implementation.
- **Luna** — excellent value; use it liberally. The obvious fit is mechanical
  work (mass edits, renames, formatting, boilerplate, running commands,
  file-location). Specifying a task tightly enough for Luna is a good way to
  handle things move. Avoid for tasks that will take some time and iteration,
  require a lot of context, or have some uncertainty or unknowns.
- For substantive plans, use an independent subagent that is a different as
  critic when the risk justifies it.

## Effort
- Medium is the floor for Sol, Opus 5, and Fable 5; high is the general
  default; extra-high is for difficult but well-specified work where deeper
  reasoning likely affects correctness (complex debugging, algorithms,
  migrations, concurrency, high-stakes review). Luna may run at any effort —
  low for pure mechanics, high or extra-high where it's doing real work; it's
  cheap enough that there's no reason to skimp.
- Fable 5: medium for ordinary planning, high for meaty, ambiguous, or
  long-horizon work.

## Provider preference
- A stated preference for one provider (Claude: Fable 5, Opus 5 / Codex: Sol,
  Luna) — usually plan capacity — is a routing constraint that outranks the
  defaults above, for the whole session. Honor it within the quality floor:
  Sol ↔ Opus 5 shifts are fine; dropping to a banned tier is not.

## Context window variants
- `[1m]` variants trade cost for a 1M-token window; the 200k variants are the
  default. Use 1M only when the task genuinely needs it (whole-repo sweeps,
  sessions expected to outlive a compaction) — not as insurance.
