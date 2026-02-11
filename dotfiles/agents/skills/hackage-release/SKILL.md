---
name: hackage-release
description: Use when user asks to release, publish, or bump version of a Haskell package to Hackage
---

# Hackage Release

Bump version, build, validate, tag, push, and publish a Haskell package to Hackage.

## Workflow

1. **Bump version** in `package.yaml` (if using hpack) or `.cabal` file
2. **Update ChangeLog.md** with release notes
3. **Regenerate cabal** (if using hpack): `hpack`
4. **Build**: `cabal build`
5. **Check**: `cabal check` (must report zero warnings)
6. **Create sdist**: `cabal sdist`
7. **Commit & tag**: commit all changed files, `git tag vX.Y.Z.W`
8. **Push**: `git push && git push --tags`
9. **Get Hackage credentials**: `pass show hackage.haskell.org.gpg`
   - Format: first line is password, `user:` line has username
10. **Publish package**: `cabal upload --publish <sdist-tarball> --username=<user> --password='<pass>'`
11. **Build & publish docs**: `cabal haddock --haddock-for-hackage` then `cabal upload --documentation --publish <docs-tarball> --username=<user> --password='<pass>'`

## Version Bumping (PVP)

Haskell uses the [Package Versioning Policy](https://pvp.haskell.org/) with format `A.B.C.D`:

| Component | When to Bump |
|-----------|-------------|
| A.B (major) | Breaking API changes |
| C (minor) | Backwards-compatible new features |
| D (patch) | Bug fixes, non-API changes |

## Nix-Based Projects

If the project uses a Nix flake, wrap cabal commands with `nix develop`:

```bash
nix develop --command cabal build
nix develop --command cabal check
nix develop --command hpack package.yaml
```

Prefer `nix develop` (flake) over `nix-shell` (legacy) to avoid ABI mismatches.

## PVP Dependency Bounds

Hackage warns about:
- **Missing upper bounds**: Every dependency should have an upper bound (e.g., `text >= 1.2 && < 2.2`)
- **Trailing zeros in upper bounds**: Use `< 2` not `< 2.0.0`; use `< 0.4` not `< 0.4.0.0`

Run `cabal check` to verify zero warnings before releasing.

## Checklist

- [ ] Version bumped in package.yaml / .cabal
- [ ] ChangeLog.md updated
- [ ] Cabal file regenerated (if hpack)
- [ ] `cabal build` succeeds
- [ ] `cabal check` reports no errors or warnings
- [ ] Changes committed and tagged
- [ ] Pushed to remote with tags
- [ ] Package published to Hackage
- [ ] Docs published to Hackage
