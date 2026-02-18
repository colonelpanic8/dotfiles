{
  description = "CI stub for the private railbird-secrets flake";

  # The real railbird-secrets flake declares these inputs; the parent flake's
  # lockfile also wires them up. Declaring them here avoids noisy warnings when
  # this stub is used via `--override-input railbird-secrets ...`.
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    agenix.url = "github:ryantm/agenix";
  };

  outputs = { self, nixpkgs, flake-utils, agenix }: {
    # Keep this minimal: NixOS flake currently doesn't use railbird-secrets, but
    # Nix will still try to fetch/resolve all inputs during evaluation.
    keys = {
      kanivanKeys = [ ];
    };
  };
}
