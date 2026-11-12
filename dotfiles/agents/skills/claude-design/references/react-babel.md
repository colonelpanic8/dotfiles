# React + Babel setup for inline JSX prototypes

For interactive prototypes and animations, you'll often want React with inline JSX rather than a build step. This works — but there are three traps that silently break things and one that wastes a lot of time if you don't know it up front.

## Pinned versions with integrity hashes (non-negotiable)

Use these exact script tags. Don't use unpinned versions (`react@18`). Don't omit the integrity attributes. Unpinned URLs occasionally serve different builds that break hydration in weird ways, and integrity hashes protect against cached/tampered copies:

```html
<script
  src="https://unpkg.com/react@18.3.1/umd/react.development.js"
  integrity="sha384-hD6/rw4ppMLGNu3tX5cjIb+uRZ7UkRJ6BPkLpg4hAu/6onKUg4lLsHAs9EBPT82L"
  crossorigin="anonymous"
></script>
<script
  src="https://unpkg.com/react-dom@18.3.1/umd/react-dom.development.js"
  integrity="sha384-u6aeetuaXnQ38mYT8rp6sbXaQe3NL9t+IBXmnYxwkUI2Hw4bsp2Wvmx4yRQF1uAm"
  crossorigin="anonymous"
></script>
<script
  src="https://unpkg.com/@babel/standalone@7.29.0/babel.min.js"
  integrity="sha384-m08KidiNqLdpJqLq95G/LEi8Qvjl/xUYll3QILypMoQ65QorJ9Lvtp2RXYGBFj1y"
  crossorigin="anonymous"
></script>
```

Load Babel *after* React and ReactDOM so the JSX scripts can reference them.

## Importing JSX files

Use `<script type="text/babel" src="...">` for JSX. Do **not** use `type="module"` — Babel-standalone doesn't play well with ES module semantics in all browsers, and you'll hit silent `<script>` evaluation failures.

```html
<script type="text/babel" src="components.jsx"></script>
<script type="text/babel" src="app.jsx"></script>
```

## Trap 1: style-object name collisions break the app silently

Every `<script type="text/babel">` file is transpiled and evaluated in the *same global scope*. If two files both define `const styles = { ... }` at the top level, the second one silently redeclares the first. Depending on which ran first, some components render with the wrong style object — or throw `Cannot read property 'foo' of undefined`. The bug is subtle because there's no error message at the point of collision.

**Fix:** give every style object a unique name tied to the component:

```jsx
// terminal.jsx
const terminalStyles = {
  root: { fontFamily: 'monospace', background: '#111' },
  line: { color: '#0f0' },
};

const Terminal = () => <div style={terminalStyles.root}>...</div>;
```

```jsx
// sidebar.jsx
const sidebarStyles = {
  root: { width: 240, background: '#1a1a1a' },
};

const Sidebar = () => <div style={sidebarStyles.root}>...</div>;
```

Or use inline styles. Just never use a bare `const styles` in more than one file.

## Trap 2: Babel scripts don't share scope

Each `<script type="text/babel">` gets its own transpile context, and the transpiled code is evaluated in a way that doesn't cleanly share locally-declared `const`/`let` across scripts. A component defined in `components.jsx` is not magically available to `app.jsx` just because both scripts loaded.

**Fix:** explicitly export components to `window` at the end of each shared file:

```jsx
// components.jsx
const Terminal = (props) => { /* ... */ };
const Line = (props) => { /* ... */ };
const Spacer = () => <div style={{ height: 12 }} />;
const Gray = ({ children }) => <span style={{ color: '#888' }}>{children}</span>;

// Make them available to other babel scripts:
Object.assign(window, { Terminal, Line, Spacer, Gray });
```

```jsx
// app.jsx
const { Terminal, Line, Spacer, Gray } = window;

const App = () => (
  <Terminal>
    <Line><Gray>$</Gray> hello</Line>
    <Spacer/>
  </Terminal>
);

ReactDOM.render(<App/>, document.getElementById('root'));
```

This pattern is ugly but reliable. Treat `window.*` as your ad-hoc module system.

## Trap 3: `scrollIntoView` breaks embedded previews

`element.scrollIntoView()` can scroll the entire embedding web app (not just the iframe), which causes the preview container to jump around. This is surprising and hard to debug once it happens.

**Fix:** use `element.scrollTop = n`, `element.scrollTo(...)`, or `parent.scrollTop = ...`. Operate on the specific scroll container, not via `scrollIntoView`.

## Trap 4: integrity hashes must match exactly

If you typo the integrity attribute, the browser silently refuses to load the script and React is undefined. The error you see is `React is not defined` inside your JSX, which misleads you into thinking the JSX is wrong.

**Fix:** copy the integrity hashes byte-for-byte from above. If you're using a different version of React/ReactDOM/Babel for some reason, fetch the correct integrity hash from unpkg (via `https://unpkg.com/react@X.Y.Z/umd/react.development.js.map` or SRI generators) rather than guessing.

## Project structure for prototypes

A clean layout for a multi-file React prototype:

```
prototype/
├── index.html       # pinned React/Babel loaders + script tags for components & app
├── components.jsx   # small reusable components (Button, Card, Modal...)
├── app.jsx          # screens and top-level layout
├── styles.css       # CSS reset + font imports + global styles
└── assets/
    ├── logo.svg
    └── hero-placeholder.svg
```

Files over ~1000 lines are hard to edit reliably with targeted edits. When one file crosses that threshold, split by domain (each screen to its own file, or extract a group of related components).

## Helpful imports for prototypes

```html
<!-- Tailwind via CDN, if you want utility classes inline -->
<script src="https://cdn.tailwindcss.com"></script>

<!-- Motion / animation -->
<script src="https://unpkg.com/popmotion@11.0.5/dist/popmotion.min.js"></script>

<!-- Icons via lucide -->
<script src="https://unpkg.com/lucide@latest"></script>
```

Tailwind via CDN is fine for prototypes — don't bother with a build step. If you use it, put your custom theme in a `<script>` block before the CDN script runs:

```html
<script>
  tailwind.config = {
    theme: { extend: {
      colors: { brand: { 500: '#D97757', 600: '#B85C3E' } },
      fontFamily: { display: ['Söhne', 'sans-serif'] },
    }},
  };
</script>
<script src="https://cdn.tailwindcss.com"></script>
```

## Calling Claude from the prototype (host-specific)

Some host environments provide a `window.claude.complete(...)` helper so prototypes can call a model without an SDK or API key. This is host-specific — don't assume it's available. If it is, the typical shape:

```javascript
// Simple call
const text = await window.claude.complete("Summarize: ...");

// Messages form
const text = await window.claude.complete({
  messages: [{ role: 'user', content: '...' }],
});
```

Token cap, rate limiting, and which model runs is environment-defined. Treat the helper as best-effort and show a loading state. Wrap in a feature check:

```javascript
if (typeof window.claude?.complete === 'function') {
  // prototype uses live Claude
} else {
  // fall back to canned responses
}
```

## Summary — checklist before running a React prototype

- [ ] Pinned versions with integrity hashes for React, ReactDOM, Babel-standalone
- [ ] Babel loaded *after* React
- [ ] Every style object has a unique name (or uses inline styles)
- [ ] Shared components exported via `Object.assign(window, {...})`
- [ ] No `type="module"` on Babel script tags
- [ ] No `scrollIntoView` in component code
