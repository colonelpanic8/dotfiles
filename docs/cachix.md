# Cachix for this repo

This repo's NixOS flake lives under `nixos/`.

The workflow in `.github/workflows/cachix.yml` can build the `strixi-minaj`
system closure on GitHub Actions and push the results to a Cachix cache.

## One-time setup

1. Create a Cachix cache (on cachix.org).
2. Create a Cachix auth token with write access to that cache.
3. In the GitHub repo settings:
   - Add a repo variable `CACHIX_CACHE_NAME` (the cache name).
   - Add a repo secret `CACHIX_AUTH_TOKEN` (the write token).

After that, pushes to `master` will populate the cache.

## Using the cache locally

Option A: ad-hoc (non-declarative)

```sh
cachix use <your-cache-name>
```

Option B: declarative via flake `nixConfig` (recommended for NixOS)

1. Get the cache public key from the Cachix UI:

- Open `https://app.cachix.org/cache/<your-cache-name>#pull`
- Copy the `Public Key` value shown there.

2. Add it to `nixos/flake.nix` under `nixConfig.extra-substituters` and
   `nixConfig.extra-trusted-public-keys`.

Note: `nixos/nix.nix` sets `nix.settings.accept-flake-config = true`, so the
flake `nixConfig` is honored during rebuilds.
