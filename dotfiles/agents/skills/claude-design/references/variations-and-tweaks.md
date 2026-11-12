# Variations and tweaks — giving options the user can mix and match

Design is rarely "here's the answer" — it's "here are a few directions, let's find the right one together." Your job is to make that comparison productive. That means:

1. Produce **3+ variations** across multiple dimensions, not just minor color tweaks on the same idea.
2. Mix **by-the-book** options (safe, match existing conventions) with **novel** options (remix visual DNA, interesting layouts, unusual metaphors).
3. Present variations in a way the user can actually evaluate — side-by-side on a canvas, toggleable via tweaks, or sequential on slides.

## The variation mix

A good variation set spans the space. Don't ship three small deviations on one idea — ship explorations of genuinely different directions.

Axes to vary across:
- **Layout** — hierarchy, rhythm, density, grid structure
- **Typography** — font pairing, scale, weight contrast, case treatment
- **Color** — primary palette, background logic, accent placement
- **Imagery** — photographic vs. illustrated vs. none; full-bleed vs. contained
- **Interaction** — hover patterns, microcopy, state feedback
- **Metaphor** — how you frame the product (terminal / document / studio / workshop / assistant)
- **Advanced CSS** — texture, blend modes, clip paths, scroll-driven motion

A good spread on, say, a landing-page hero might be:

1. **A — Editorial.** Serif headline, photograph, generous negative space, quiet CTA.
2. **B — Product-forward.** Mono headline, product screenshot, tight rhythm, prominent CTA.
3. **C — Remix.** Headline as a diagram/chart visual, no traditional hero image; draws from the product's signature interaction as the hero itself.

Start basic, then get bolder. The bold options prime the user to see what's possible; the basic ones give them a safe baseline to compare against. Users often end up picking a *fusion* — basic structure, one bold element from C. That's fine. That's the point.

## Presenting variations

Pick the presentation method based on what's varying:

| Varying... | Present as... |
|---|---|
| One static element (hero, card, button) in ≥3 treatments | **Design canvas** — grid of labeled cells |
| A prototype with minor variants (dark mode, copy, layout mode) | **Tweaks panel** — single prototype, toggleable |
| A narrative (different deck outlines, different pitch angles) | **Sections** within the deck, labeled clearly |
| Major deviations in UX flow | **Separate HTML files**, linked together |

**Do not** produce N separate HTML files when one file with Tweaks would work. Copying the whole artifact per variant multiplies the cost of every future change.

## The Tweaks protocol

Tweaks is an in-artifact UI that lets the user toggle aspects of the design live: colors, typography, copy, layout variants, feature flags. When on, the panel is visible; when off, the design looks final and the controls are hidden.

### Host integration (when available)

Some host environments (like Claude.ai's artifact viewer) provide a toolbar toggle that activates/deactivates Tweaks mode and persists changes back to the file. The protocol:

**1. Register the listener first, then announce availability.** Order matters — if you `postMessage` availability before your handler exists, the host's activate message can arrive before you're ready.

```javascript
// Register the listener FIRST
window.addEventListener('message', (e) => {
  if (e.data?.type === '__activate_edit_mode') showTweaksPanel();
  if (e.data?.type === '__deactivate_edit_mode') hideTweaksPanel();
});

// THEN announce availability
window.parent.postMessage({ type: '__edit_mode_available' }, '*');
```

**2. Persist changes back to the file** with `__edit_mode_set_keys`:

```javascript
function onTweakChange(key, value) {
  applyLive(key, value);  // update the page immediately
  window.parent.postMessage({
    type: '__edit_mode_set_keys',
    edits: { [key]: value }
  }, '*');
}
```

**3. Wrap defaults in edit-mode markers** so the host can rewrite them on disk:

```javascript
const TWEAK_DEFAULTS = /*EDITMODE-BEGIN*/{
  "primaryColor": "#D97757",
  "fontSize": 16,
  "dark": false,
  "variant": "A"
}/*EDITMODE-END*/;
```

Rules for the markers:
- The block must be **valid JSON**: double-quoted keys, double-quoted strings.
- Exactly **one** such block in the root HTML file.
- It must be inside an inline `<script>` (not an external file).
- The host parses the JSON, merges your edits, and writes the file back — so changes survive reload.

### Standalone Tweaks (no host integration)

If there's no host toolbar, just render a floating panel in-page. The user toggles tweaks by clicking a button in the panel itself. Persist via `localStorage` instead of `postMessage`:

```javascript
const TWEAKS = loadTweaksFromStorage() ?? {
  primaryColor: '#D97757',
  fontSize: 16,
  dark: false,
  variant: 'A',
};

function onTweakChange(key, value) {
  TWEAKS[key] = value;
  applyLive(key, value);
  localStorage.setItem('tweaks', JSON.stringify(TWEAKS));
}
```

### Tweaks UI patterns

Keep the surface small. Tweaks is a side-channel, not the product. Good placements:

- **Floating panel in the bottom-right corner** — title it "Tweaks" (match the toolbar toggle naming if a host integration exists). Collapsible. Can be dragged.
- **Inline handles on hover** — small gear icons next to the element they control.
- **Top toolbar** — only if there are a lot of tweaks; use sparingly.

Rules:
- When Tweaks is off, the panel is entirely hidden. The design should look final.
- Don't overbuild the panel itself — no deep nesting, no tabs, no scroll. If it's getting long, you have too many tweaks.
- Group related tweaks (all color swatches together, all type controls together).

### What to expose as Tweaks

If the user specifies: exactly what they asked for, plus 1–2 bonus tweaks if interesting.

If the user didn't specify: default to 3–5 tweaks that open up interesting design space. Good defaults:
- Primary color (color picker or swatch set)
- Typography (pairing picker, or just headline-font toggle)
- Density (compact / comfortable / spacious)
- Variant switch (A / B / C — cycle through major layouts)
- Copy tone (professional / casual / punchy)
- Feature flag (show/hide optional sections)

Be creative. Tweaks is your chance to surface "I wasn't sure what you wanted so I tried three versions" — without making three files.

### Cycling between variants

A common pattern: the user asks for multiple variants of a single element inside a larger design. Instead of making N copies of the whole design, add a tweak that cycles through that one element:

```javascript
// Tweak: which hero variant to show
const [heroVariant, setHeroVariant] = useState(TWEAKS.variant);
// In the render:
{heroVariant === 'A' && <HeroEditorial/>}
{heroVariant === 'B' && <HeroProduct/>}
{heroVariant === 'C' && <HeroRemix/>}
```

This is cleaner than separate files and gives the user direct A/B/C comparison in one place.

## When you're not sure how many variations to make

Ask. Variations are the thing users most often want different amounts of. Some want 1 polished direction; some want 8 rough directions; some want 3 medium-fidelity. Ask which they want, and on which axis.

If the user hasn't answered and you need to commit: default to **3 variations**, spanning conservative → novel, with a visible note that you can add more in any direction.
