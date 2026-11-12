# Workflow — asking good questions and gathering context

Good design starts before the first pixel. Two things determine whether the artifact will be any good: (1) did you understand what the user actually wants, and (2) did you gather the visual context to ground the work in reality.

## When to ask questions vs. just build

Not every task needs a question round. Use this guide:

| Situation | What to do |
|---|---|
| "Make a deck for the attached PRD" | Ask — audience, tone, length, output format are all undefined |
| "Make a deck with this PRD for Eng All Hands, 10 min" | Build — enough is given |
| "Turn this screenshot into an interactive prototype" | Ask only if intended behavior is unclear from the image |
| "Make 6 slides on the history of butter" | Ask — vague |
| "Prototype an onboarding for my food delivery app" | Ask a LOT |
| "Recreate the composer UI from this codebase" | Build — the source is the brief |
| "Change the CTA to navy" (follow-up) | Build — small tweak |
| "Fix the spacing on slide 3" | Build — localized |

**Rule of thumb:** ask when the design space has too many free variables (audience, tone, fidelity, variations, platform, brand). Build when the constraints are already pinned down, or when the ask is a localized tweak.

## The question checklist

When you do ask, cover these in a single round. Asking 4–10 focused questions up front is faster than asking 2 at a time across three rounds.

### 1. Starting point and product context (always ask)

You cannot design well in a vacuum. Confirm with a direct question:

> "What should I design from? Options: a codebase (I can read tokens and components), an existing design system or UI kit, screenshots of the current product, a Figma file, or a brand doc. If none of these exist yet, do you want to pick a reference brand as a starting point?"

If the user has nothing, their answer should be a named reference ("design it in the spirit of Linear") or a commitment that you're designing from scratch with a clear aesthetic direction (see the `frontend-design` skill for that).

### 2. Output and fidelity

- What format? (deck, landing page, clickable prototype, animation, poster, wireframe, design canvas comparing options)
- What fidelity? (wireframe / mid-fi / hi-fi)
- What platform / viewport? (mobile, desktop, 1920×1080 slide, print, social card)
- Will this be presented, handed to a developer, shipped to end users, printed?

### 3. Variations

Critical. If you don't ask, you'll under-deliver or over-deliver on variation count.

- How many variations of the overall flow do you want? (1, 3, 5+)
- Variations on which axis? (flow/UX, visual style, copy, motion, color, typography)
- Do you want divergent ideas or conservative variations on a single direction?
- Any specific screens/elements that need multiple options?

### 4. Tone and content

- Who's the audience?
- Serious / playful / technical / editorial / quiet / loud?
- Do you have copy, or should I write placeholder copy in a specific voice?

### 5. Tweaks

- What do you want to be able to tweak in-design? (colors, typography, copy, layout variants, feature flags)
- Is a floating Tweaks panel OK, or would you rather see options as separate slides?

### 6. Problem-specific (always add 3–5 more)

Look at the specific ask and generate problem-specific questions. For example:
- For a pricing page: "How many tiers? Monthly/annual toggle? Feature comparison matrix or per-tier lists?"
- For an onboarding: "Step count? Skip button? Permission prompts included? Empty-state for first use?"
- For an infographic: "Data source? Primary stat vs. supporting stats? Print-ready or screen-only?"
- For a landing page: "Hero format? Social proof? CTA destination? Fold boundary matter?"

**Tip:** if you find yourself writing only 2–3 questions, you're probably missing problem-specific ones. Push to 8–10.

## Gathering design context

Once the user points you at a codebase / design system / Figma:

1. **List the relevant files.** Don't try to read everything — find the 5–15 files that define the design language: theme tokens, global styles, primary components, one representative page.
2. **Read the tokens first.** Color variables, spacing scale, type scale, radii, shadows. These are the *atoms* — every other choice builds on them.
3. **Copy needed assets into the project.** Don't reference them across project boundaries; copy the specific files you need. Bulk-copying a whole component library is wasteful — be surgical.
4. **Read representative components.** How does the product do buttons, inputs, cards, empty states? Match the conventions.
5. **Narrate what you see.** A short "Here's the visual language I'm working with: [type scale] / [primary + neutral palette] / [layout rhythm] / [signature interactions]" before building prevents a lot of rework.

If the user linked a GitHub repo or Figma file, import the relevant pieces. Don't skim file names and design from memory — file names are a menu, not the meal. Read the actual source.

## Showing early, iterating fast

Your first HTML artifact should land within the first few tool calls — even if most of it is placeholders and a top-level "Here's my design reasoning" block. Treat the first output as a draft you and the user discuss, not as a finished deliverable. Polish comes from iteration, not from one heroic render.

Good first-artifact pattern:

```html
<body>
  <main class="sketchpad">
    <section class="assumptions">
      <h1>Design notes — draft v1</h1>
      <p>Working assumptions: {audience}, {tone}, {platform}.
         Visual system: {type scale}, {palette}, {layout}.
         Variations planned: A (conservative, matches existing), B (bolder type treatment),
         C (remix: {metaphor}).</p>
      <p class="todo">Waiting on: {logo asset}, {real copy for hero}.</p>
    </section>
    <!-- Variations render below; placeholders until context is in -->
  </main>
</body>
```

Show this to the user, confirm direction, then fill in. Working this way surfaces misalignments before you've invested in the wrong direction.
