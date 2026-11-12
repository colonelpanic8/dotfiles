# Design principles — how to avoid AI-slop and produce craft

"AI-slop" is a specific look: over-relying on a narrow set of visual patterns that read as machine-generated. Avoiding it is not about being clever — it's about having a clear system and committing to it. Below: the anti-slop rules, the craft rules, and the content rules. Read all three.

## Anti-slop rules

These are the patterns that instantly date a design as "AI made this":

**No aggressive gradient backgrounds.** Especially not purple-to-blue, sunset, or conic rainbows. If you need a background with visual interest, use: a solid brand color, a subtle single-hue gradient (<10° hue variance), a muted texture, or a full-bleed photograph. Gradients as decoration — avoid.

**No emoji unless the brand uses them.** Emoji in headlines or as bullet markers is a tell. If the brand's real communication channels don't use emoji, don't introduce them. When you'd reach for an emoji, draw a real icon or use a placeholder and ask the user to provide one.

**No rounded-corner cards with a left-border accent stripe.** Especially paired with a muted icon and a gradient background. This is the most-generated "dashboard card" in the world. If you're making a dashboard or feature callout, find a different container metaphor — a full-bleed panel, a numbered sequence, a framed cell, a hand-drawn outline, a ticket / receipt shape, an overlapping duo, whatever the brand suggests.

**No SVG-drawn imagery as a substitute for real assets.** Don't try to SVG-draw a hero illustration, a product screenshot, a portrait, or a 3D object. Drawing "the product" in SVG always looks like a diagram, not a product. Use placeholders (`[hero: product running in browser, 1400×900]`) and ask for real materials.

**No CSS silhouettes standing in for real product shots.** A specific and common failure: designing a launch animation or hero for a physical product (a phone, a camera, a headset) and representing the product with a CSS-drawn silhouette or gradient-filled shape. This is the *exact* signature of "generic AI tech animation" — every brand ends up looking the same because no brand actually shows up. Go get the real product render via the Core Asset Protocol. If you truly cannot, show a labeled placeholder and tell the user, don't silhouette over it.

**No decorative gradient orbs "representing AI".** The floating purple-to-pink gradient orb as a stand-in for "AI magic" is the single most over-used signifier in contemporary tech design. If your concept calls for "AI", find a less-worn metaphor: a diagram, a waveform, a specific product surface, a typeset word. Gradient orbs read as lazy.

**No overused font stacks.** Inter, Roboto, Arial, system-ui, Fraunces, and other defaults are rarely the right answer. If you have a brand font, use it. If you have free range, pick a pairing with deliberate voice — something like JetBrains Mono + Söhne, Tiempos + Inter Display, PP Neue Montreal + Commit Mono, IBM Plex Sans + Plex Serif — and commit. If you don't know good type pairings, ask the user or reach for a reference brand's system.

**No decoration-by-dataviz.** Fake stats, invented numbers, decorative charts that don't represent anything — these are slop, not design. Every number on screen should mean something.

**No "3 column feature grid" as the default page structure.** The landing-page pattern of hero → three-column feature grid → testimonials → CTA is the pre-trained path of least resistance. When a brief calls for a landing page, consider: a single-column editorial narrative, a comparison table, a full-bleed product demo, a stacked case-study format, an interactive exploration. The three-column grid is sometimes right — but pick it because it fits, not because it's what you reach for first.

**No over-iconified bullet lists.** Bullet lists with an icon per item (often a pastel circle with a tiny symbol) rarely add clarity — they add noise. A plain bullet list or a numbered list reads faster in most cases. Use iconography when the icon carries real signal (e.g., file-type indicators in a list of documents), not as a bullet replacement.

## Craft rules

These separate competent design from student work:

**Commit to a system before placing pixels.** State (in a comment or a visible draft note at the top of the HTML) your type scale, your 1–2 background colors, your layout rhythm, your section-header pattern. Then hold yourself to it. Consistency is the single biggest difference between a polished artifact and an amateurish one.

**Scale floors are real:**
- 1920×1080 slides: body text ≥ 24px. Headers much larger. 12px on a slide is illegible from the back of a room.
- Print: ≥ 12pt body.
- Mobile: hit targets ≥ 44px × 44px. Hairlines at <1px stop rendering on subpixel devices.

**Use modern CSS.** You can do more than you think with:
- `text-wrap: pretty` and `text-wrap: balance` — instant typographic polish on headlines and paragraphs
- CSS Grid and `subgrid` — real layouts without wrapper hell
- `oklch()` — pick harmonious color variants without inventing new hues
- Container queries — variants that respond to their container, not the viewport
- `@scope` and `@layer` — style without specificity wars
- `view-timeline` and `animation-timeline: scroll()` — scroll-driven motion without JS
- CSS nesting — readable style structures

If you're reaching for three `<div>` wrappers to force a layout, there's a one-line Grid solution. Think before nesting.

**Color from existing brand / system first.** If a brand doc or token file exists, use those exact hex values. If you need a derived color (hover, disabled, surface-2), use `oklch()` to keep it harmonious rather than inventing a new hue. Inventing colors from thin air produces discordant palettes.

**Placeholders beat fakes.** If you don't have an icon, a logo, a headshot, a product screenshot, or a chart — draw a labeled placeholder and clearly mark it as such. Do not try to fake the real thing. Placeholders are honest and unambiguous; fakes are distracting and sometimes offensive (e.g., fake faces, fake logos, fake data).

**Use visual rhythm.** Consider how the artifact reads top-to-bottom or left-to-right. Alternate heavy and light sections. Give full-bleed imagery room to breathe. Use 1–2 background colors across a deck (not 10) so different backgrounds *mean something*. Silence between sections is a design tool, not wasted space.

**Match what's there.** If you're adding to an existing UI: spend a minute observing the visual vocabulary before writing code. Note copywriting tone, color palette, density, card/shadow conventions, hover/click states, animation styles. Then match them. It can help to "think out loud" in prose: "Observed: tight 4/8 spacing grid, near-black on off-white, only one shadow depth, never uses icons inside buttons." Then build to that observation.

## Content rules

These are the rules designers forget and copywriters never would:

**No filler content.** Every element should earn its place. Don't pad a design with:
- Dummy sections ("Our values", "Why choose us", "Team section" when the user didn't ask)
- Placeholder paragraphs beyond what you need to show the layout
- Decorative stats / numbers / icons with no meaning ("data slop")
- "Testimonial" sections without real testimonials
- Feature grids to fill the middle of a landing page

If a section feels empty, that's a design problem — solve it with composition, scale, full-bleed imagery, intentional negative space, or a bigger hero. Not with invented content.

**One thousand no's for every yes.** Before adding something, ask: is this here because it's needed, or because the layout looked sparse? If the latter: delete it and make the existing content bigger, bolder, or rebalanced.

**Ask before adding material.** If you think an additional section / page / copy block would strengthen the design: ask the user first. They know their audience and goals better than you do. Expand on demand, not by default.

**Use the user's voice.** If they gave you copy, use their exact wording. If you're writing placeholder copy, match the register they've used in the brief. Don't invent slick marketing-speak when the product is technical and understated (or vice versa).

**Avoid unnecessary iconography.** Icons should clarify or signify, not decorate. A bullet list with an icon per line is usually noisier than a plain bullet list. When in doubt, remove the icon and see if anything is lost.

## Design-system thinking

When the user asks for a system of related artifacts (a deck, a multi-screen prototype, a landing-page plus email), treat the *system* as the product, not the individual screens:

- Pick 1–2 background colors for the whole system. Use them with intent (section starters get the accent background, body content gets the neutral).
- Pick a type scale (e.g., 72 / 48 / 32 / 20 / 16 / 13) and use those exact sizes. No 28px just because it "felt right."
- Pick a spacing grid (4 / 8 / 12 / 16 / 24 / 32 / 48 / 64) and use those exact values.
- Pick a single motion language (same easing, same duration range) and reuse it.

A coherent system lets you take risks on individual screens because the system carries the through-line.

## Final gut check

Before calling it done, skim the artifact as if you've never seen it and ask:
- Does this look like it came from a real, specific designer — or like it could have come from any AI?
- Is there a clear point of view, or did I hedge every decision?
- Is there one thing here a user would remember?

If the answer to any of these is "generic," rebalance toward specificity: pick the bolder color, commit to the heavier type weight, make the hero bigger, remove the decorative second section. A distinctive imperfect artifact beats a "safe" forgettable one.
