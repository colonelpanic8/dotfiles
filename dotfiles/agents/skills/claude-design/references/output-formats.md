# Output formats — picking and building each type

The format is a function of the exploration. Use the decision tree up front, then build from the matching skeleton below.

## Decision tree

```
Is the exploration primarily visual (color, type, static layout)?
├─ Yes, show options side-by-side → DESIGN CANVAS
└─ No ↓

Is this a narrative sequence (story, pitch, tutorial)?
├─ Yes → SLIDE DECK
└─ No ↓

Is there interaction / flow / many options?
├─ Yes, high fidelity → INTERACTIVE PROTOTYPE (with Tweaks for variants)
└─ No ↓

Is this about motion / timing / video?
├─ Yes → TIMELINE ANIMATION
└─ No ↓

Lots of rough ideas, early in exploration?
└─ Yes → WIREFRAMES / STORYBOARD
```

## 1. Design canvas — multiple static options side-by-side

**When:** pure visual exploration where the user wants to compare variants at a glance. Hero layouts, color treatments, type pairings, button styles, card densities.

**Skeleton:** one HTML file, a grid container, each cell clearly labeled with the variant name and what it's exploring.

```html
<!doctype html>
<html>
<head>
  <meta charset="utf-8">
  <title>Hero — variations</title>
  <style>
    body { margin: 0; font-family: 'Söhne', sans-serif; background: #f4f1ec; }
    .canvas { display: grid; grid-template-columns: repeat(2, 1fr); gap: 48px; padding: 48px; }
    .cell { background: white; border-radius: 8px; overflow: hidden; }
    .cell-label { padding: 16px 20px; border-bottom: 1px solid #eee; }
    .cell-label h3 { margin: 0; font-size: 13px; letter-spacing: 0.04em; text-transform: uppercase; color: #666; }
    .cell-label p { margin: 4px 0 0; font-size: 14px; color: #999; }
    .cell-body { padding: 0; }
  </style>
</head>
<body>
  <main class="canvas">
    <section class="cell">
      <header class="cell-label">
        <h3>Variant A · Editorial</h3>
        <p>Serif headline, generous negative space, full-width photo</p>
      </header>
      <div class="cell-body"><!-- hero A --></div>
    </section>
    <section class="cell">
      <header class="cell-label">
        <h3>Variant B · Product-forward</h3>
        <p>Mono headline, product screenshot, tight rhythm</p>
      </header>
      <div class="cell-body"><!-- hero B --></div>
    </section>
    <!-- C, D ... -->
  </main>
</body>
</html>
```

**Gotchas:**
- Each cell label should describe *what's being explored* ("editorial tone"), not just "Variant A."
- Keep cells the same size; inconsistent cell dimensions make variants hard to compare.
- Use a neutral, unopinionated canvas background (cream, light grey) so the variant colors read correctly.
- 2–3 columns is usually right; 4+ variants per row becomes too small to read.

See [assets/design-canvas.html](../assets/design-canvas.html) for a ready-to-use starter.

## 2. Slide deck — paged narrative

**When:** pitch, talk, report, walk-through, class. Anything sequential where each slide has a single job and a "next" pressed between them.

**Skeleton:** a fixed-size canvas (default 1920×1080, 16:9) wrapped in a stage that scales it to fit the viewport, with controls outside the scaled element.

```html
<!doctype html>
<html>
<head>
  <meta charset="utf-8">
  <title>Deck — Project Aurora</title>
  <link rel="stylesheet" href="deck.css">
</head>
<body>
  <deck-stage width="1920" height="1080">
    <section data-screen-label="01 Title">...</section>
    <section data-screen-label="02 Problem">...</section>
    <section data-screen-label="03 Solution">...</section>
    <!-- one <section> per slide, in order -->
  </deck-stage>

  <!-- Optional: speaker notes, read by the stage if present -->
  <script type="application/json" id="speaker-notes">
    ["Slide 1 notes...", "Slide 2 notes...", ""]
  </script>

  <script src="deck_stage.js"></script>
</body>
</html>
```

**Must-have behaviors:**
- **Scaling:** the 1920×1080 canvas scales via `transform: scale()` to fit any viewport; letterboxed on black.
- **Navigation:** arrow keys, tap zones, and visible prev/next controls. Controls live *outside* the scaled element so they stay usable on small screens.
- **Slide counter:** a `{current}/{total}` overlay. 1-indexed (slide 1, not slide 0).
- **Persistence:** slide index in `localStorage` so refresh doesn't lose position.
- **Screen labels:** each slide gets a `data-screen-label` like "01 Title" (1-indexed, matching the visible counter). Enables comment context on specific slides.
- **Print-to-PDF:** each slide prints to its own page (via `@page size: 1920px 1080px` and `break-after: page`).

**Scale floors on slides:**
- Body text: ≥ 24px (ideally 32–48px for emphasis)
- Headers: ≥ 64px
- Don't crowd. If a slide has more than ~40 words of body copy, split it or move it to speaker notes.

**Speaker notes:** only add them if the user asks. When you do, put a full conversational script (what to say) in a `<script type="application/json" id="speaker-notes">` block. Use speaker notes to reduce text-on-slide and lean into impactful visuals.

See [assets/deck-stage.html](../assets/deck-stage.html) for a minimal working skeleton with scaling and navigation.

## 3. Interactive prototype — clickable hi-fi

**When:** user needs to feel the interaction, not just see the layout. Onboarding flows, multi-step forms, navigation patterns, micro-interactions.

**Skeleton:** React (inline JSX with Babel) for stateful components, plus a device frame if it's a mobile/desktop app mockup.

```html
<!doctype html>
<html>
<head>
  <meta charset="utf-8">
  <title>Onboarding — prototype</title>
  <!-- Pinned React + Babel with integrity hashes (see references/react-babel.md) -->
  <script src="https://unpkg.com/react@18.3.1/umd/react.development.js" integrity="sha384-hD6/rw4ppMLGNu3tX5cjIb+uRZ7UkRJ6BPkLpg4hAu/6onKUg4lLsHAs9EBPT82L" crossorigin="anonymous"></script>
  <script src="https://unpkg.com/react-dom@18.3.1/umd/react-dom.development.js" integrity="sha384-u6aeetuaXnQ38mYT8rp6sbXaQe3NL9t+IBXmnYxwkUI2Hw4bsp2Wvmx4yRQF1uAm" crossorigin="anonymous"></script>
  <script src="https://unpkg.com/@babel/standalone@7.29.0/babel.min.js" integrity="sha384-m08KidiNqLdpJqLq95G/LEi8Qvjl/xUYll3QILypMoQ65QorJ9Lvtp2RXYGBFj1y" crossorigin="anonymous"></script>
</head>
<body>
  <div id="root"></div>
  <script type="text/babel" src="components.jsx"></script>
  <script type="text/babel" src="app.jsx"></script>
</body>
</html>
```

**Rules:**
- Center the prototype in the viewport, or fill the viewport with reasonable margins. Don't add a "title screen" wrapper around the prototype — dive straight into the product.
- If it's a mobile UI mockup, wrap it in an iOS or Android device frame (see [assets/device-frames.md](../assets/device-frames.md)).
- Real state, not `window.alert`. If the user can click through a sign-up flow, the flow should actually advance state.
- Mock the backend inline — `const users = { ... }; const login = (u, p) => users[u]?.password === p`. Don't call out to a real API.
- Never use `scrollIntoView` — it can disrupt embedding in preview environments. Use `element.scrollTop = ...` or `element.scrollTo(...)` instead.

For variants of a prototype (dark mode, different copy, alt layouts), use the **Tweaks** protocol — see [references/variations-and-tweaks.md](variations-and-tweaks.md).

## 4. Timeline animation — motion design

**When:** animating a concept, demoing an interaction in motion, producing a short video-style artifact.

**Skeleton:** a Stage component with a scrubber + play/pause, and Sprite components with start/end keyframes.

```jsx
// animations.jsx — conceptual shape (see assets/animations.jsx for a working starter)
const { useTime } = window.AnimationStage;

const Intro = () => {
  const t = useTime();  // 0..duration in seconds
  const opacity = interpolate(t, [0, 0.5], [0, 1], Easing.outCubic);
  return <div style={{ opacity }}>Hello</div>;
};

ReactDOM.render(
  <Stage duration={6.0}>
    <Sprite start={0} end={2}><Intro/></Sprite>
    <Sprite start={1.5} end={4}><Middle/></Sprite>
    <Sprite start={3.5} end={6}><Outro/></Sprite>
  </Stage>,
  document.getElementById('root')
);
```

**Rules:**
- Use one motion language across the animation (same easing curves, consistent duration scale). Mixing easings reads as undirected.
- Don't add a "title card" unless the piece is ≥ 30 seconds. Short animations should just start.
- Time markers in `Sprite start={…} end={…}` are in seconds, 1-indexed intuitively (0 = frame at t=0, 1.5 = frame at 1.5s).
- Persist the scrubber position in `localStorage` so refresh doesn't lose position (iteration workflow).
- If the animation has no interaction, loop it automatically when it reaches the end.

Fall back to Popmotion or GSAP only when the Stage + Sprite pattern can't express what you need. Usually it can.

See [assets/animations.jsx](../assets/animations.jsx) for a working Stage/Sprite implementation.

## 5. Wireframes / storyboard

**When:** breadth over polish. Early exploration, lots of ideas cheap, decisions are still open.

**Skeleton:** a simple grid of low-fidelity frames. Black/white or a single accent. Thin rectangles, placeholder boxes with labels, handwritten-feeling annotations.

```html
<main style="display: grid; grid-template-columns: repeat(3, 1fr); gap: 24px; padding: 48px; background: #fafafa;">
  <figure class="wire">
    <h3>01 · Landing</h3>
    <!-- inline SVG or simple divs for rectangles -->
    <div class="wire-frame">
      <div class="wire-nav"></div>
      <div class="wire-hero">[hero image]</div>
      <div class="wire-cta">Sign up</div>
    </div>
    <figcaption>User lands, sees value prop, clicks Sign up</figcaption>
  </figure>
  <!-- 02 Sign up, 03 Onboarding, ... -->
</main>
```

**Rules:**
- Keep it actually low-fidelity. If you find yourself adding colors, shadows, and typography — you're building a mid-fi mockup, which is a different artifact.
- Each frame gets a number, a title, and a one-line caption describing what happens or what the user thinks.
- 6–12 frames per storyboard is a good range. More than that and you should split into multiple flows.

## Fixed-size content — scaling

Anything with a fixed canvas size (slide decks, animations, posters) must scale to fit any viewport. Never assume the user views at 1:1.

The pattern:
1. A fixed-size inner element (e.g., `1920 × 1080`).
2. An outer "stage" that fills the viewport and letterboxes on black.
3. JS that measures the viewport, computes `scale = min(vw/1920, vh/1080)`, and applies `transform: scale(${scale})` to the inner element.
4. Controls (prev/next, play/pause, scrubber) live *outside* the scaled element so they stay at consistent size.

Don't hand-roll this for decks or animations — use the starters in [assets/](../assets/). They cover scaling, keyboard navigation, localStorage persistence, and print-to-PDF.

## Linking between pages

If you produce multiple HTML files (one per screen, one per section), link them with standard `<a>` tags and relative URLs: `<a href="screens/02-onboarding.html">Next →</a>`. Keep them in the same directory tree so the links resolve when the folder is moved.
