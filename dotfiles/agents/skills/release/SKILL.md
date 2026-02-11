---
name: release
description: Use when user asks to release, publish, bump version, or prepare a new version for deployment
---

# Release

Validate, format, bump version, and tag for release.

## Workflow

1. **Validate** - Run project's validation command
2. **Fix formatting** - Auto-fix prettier/formatting issues if any
3. **Bump version** - Ask user for bump type, update package.json
4. **Commit & tag** - Commit version bump, create git tag
5. **Optionally push** - Ask if user wants to push

## Commands

```bash
# 1. Validate
yarn validate  # or: npm run validate

# 2. Fix formatting if needed
yarn prettier:fix  # or: npm run prettier:fix

# 3. Bump version (edit package.json)
# patch: 1.2.3 → 1.2.4
# minor: 1.2.3 → 1.3.0
# major: 1.2.3 → 2.0.0

# 4. Commit and tag
git add package.json
git commit -m "chore: bump version to X.Y.Z"
git tag vX.Y.Z

# 5. Push (if requested)
git push && git push --tags
```

## Quick Reference

| Bump Type | When to Use |
|-----------|-------------|
| patch | Bug fixes, small changes |
| minor | New features, backwards compatible |
| major | Breaking changes |

## Before Release Checklist

- [ ] All tests pass
- [ ] No lint errors
- [ ] Formatting is clean
- [ ] Changes are committed
