# Verification — checking the artifact actually works

Claiming "done" without loading the HTML in a real browser is a guess, not a delivery. A design artifact that looks right in your head but 404s on a font, throws a React error, or renders unscaled on a small viewport is worse than shipping nothing — the user wastes a round-trip fixing it.

## The verification loop

For every design artifact, before you declare done:

1. **Open the HTML in a real browser.** Use whichever entry point your environment provides: the host's built-in preview/iframe, `/browse` (if gstack is installed), `mcp__claude-in-chrome__*`, or `mcp__computer-use__*`. Either way: a real browser load is required.
2. **Read the browser console.** Any 404s, JS errors, or React mount warnings mean something is broken. Fix them. Re-verify.
3. **Test the scaling.** For fixed-size content (decks, animations, posters), resize the preview pane or viewport to multiple sizes. The content should letterbox cleanly and controls should remain usable.
4. **Click through at least one flow.** For interactive prototypes, click the primary path — sign-up, onboarding, core interaction. Don't trust that click handlers work because "the code looks right."
5. **Check font loading.** Web fonts loaded from CDN can silently 404. If you see system-font fallback where you expected a brand font, investigate.

Only after these five checks pass should you tell the user the artifact is ready.

## What "console clean" means

A clean console has:
- No `Failed to load resource: 404` entries
- No red `Uncaught …` errors
- No React "Warning: Each child in a list should have a unique key" warnings (these cause silent bugs in reconciliation)
- No CORS or CSP errors on font/image fetches
- No mixed-content warnings (HTTP resources on an HTTPS page)

Yellow dev warnings about specific React patterns (deprecated APIs, etc.) are acceptable if they're not actually affecting behavior. But if you see *any* red error, fix it — even if the page seems to render.

## Format-specific checks

### Slide decks

- [ ] Prev/next arrows and keyboard navigation both work
- [ ] Slide counter updates with each navigation
- [ ] Slide index persists in localStorage (refresh → same slide)
- [ ] Scaling letterboxes cleanly on viewports narrower than 1920px
- [ ] Text is ≥ 24px body / ≥ 64px headers
- [ ] Each slide has a `data-screen-label` like "01 Title" (1-indexed)
- [ ] Speaker notes (if present) sync to slide index via `postMessage`

### Interactive prototypes

- [ ] Primary flow advances end-to-end without console errors
- [ ] Form inputs accept text and update state visibly
- [ ] Hover / focus / active states are defined (not just default browser styles)
- [ ] No `scrollIntoView` in the codebase
- [ ] Device frame (if used) matches the platform (iOS frame for iOS UI, Android for Android)

### Timeline animations

- [ ] Scrubber moves smoothly across the full duration
- [ ] Play/pause button toggles playback
- [ ] Scrubber position persists in localStorage
- [ ] No frame drops visible during playback (if possible to tell by eye — if unsure, record and watch at 0.5x)
- [ ] Animation loops or holds its final frame at the end (not a frozen intermediate state)

### Design canvas

- [ ] All cells have labels describing what's being explored
- [ ] Cells are consistent in size (unless the variant is about size)
- [ ] Canvas background doesn't interfere with the variant colors
- [ ] No overflow that crops content inside cells

## When verification fails

If a check fails, fix the root cause. Don't:
- Hide the broken element with `display: none`
- Swallow a JS error with try/catch "for now"
- Remove a font import because it 404'd
- Skip a failed click-through assertion

Each of these is a silent "I'll fix it later" that makes the artifact brittle. Root-cause the issue (wrong font URL, bad component prop, Babel scope bug) and fix it properly.

## When the user asks for a mid-task check

If the user says "can you screenshot and check the spacing on slide 3" or "does the hover state work," don't wait to verify at the end — verify now, report back, continue. Targeted mid-task verification catches issues while the context is fresh.

## Don't over-verify speculatively

Don't grab screenshots just because. Don't click through every screen of a prototype when the user asked you to fix one CSS value. Verification should match the change: a local CSS edit needs a local visual check; a new interaction flow needs a click-through; a new deck scaffold needs a full pass.

Over-verification wastes context and the user's patience. Under-verification ships broken work. Match the check to the change.

## Tooling notes

Pick the verification entry point based on what your environment provides:

- **Host built-in preview** — if your host has its own preview/iframe pane, it's often the fastest path.
- **`/browse`** (gstack, if installed) — fast headless browser, DOM-aware, takes screenshots, runs JS probes.
- **`mcp__claude-in-chrome__*`** — real Chrome; useful for logged-in sessions and complex state.
- **`mcp__computer-use__*`** — desktop-level automation; note tier-"read" for browsers means you can screenshot but not click.

Whichever entry point you use, the requirement is the same: actually load the page in a real browser and observe it.
