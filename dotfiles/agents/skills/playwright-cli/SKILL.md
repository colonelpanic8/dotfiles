---
name: playwright-cli
description: Automate browser interactions from the shell using Playwright via the `playwright-cli` command (open/goto/snapshot/click/type/screenshot, tabs/storage/network). Use when you need deterministic browser automation for web testing, form filling, screenshots/PDFs, or data extraction.
---

# Browser Automation With playwright-cli

This system provides `playwright-cli` via Nix (see `nixos/overlay.nix` and `nixos/code.nix`), so it’s available on `PATH` without any `npm -g` installs.

## Quick Start

```bash
# First run (downloads browser bits used by Playwright)
playwright-cli install-browser

# Open a new browser session (optionally with a URL)
playwright-cli open
playwright-cli open https://example.com/

# Navigate, inspect, and interact
playwright-cli goto https://playwright.dev
playwright-cli snapshot
playwright-cli click e15
playwright-cli type "search query"
playwright-cli press Enter

# Save artifacts
playwright-cli screenshot --filename=page.png
playwright-cli pdf --filename=page.pdf

# Close the browser
playwright-cli close
```

## Practical Workflow

1. `playwright-cli open` (or `open <url>`)
2. `playwright-cli snapshot`
3. Use element refs (`e1`, `e2`, ...) from the snapshot with `click`, `fill`, `hover`, `check`, etc.
4. Take `screenshot`/`pdf` as needed
5. `playwright-cli close`

## Tips

- Use `playwright-cli state-save auth.json` / `state-load auth.json` to persist login state across runs.
- Use named sessions with `-s=mysession` when you need multiple concurrent browsers.
- Set `PLAYWRIGHT_CLI_PACKAGE` to pin the npm package (default is `@playwright/cli@latest`).
