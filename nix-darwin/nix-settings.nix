{
  experimental-features = "nix-command flakes";
  # Trigger store GC before Nix consumes the machine's build headroom.
  min-free = 10 * 1024 * 1024 * 1024;
  max-free = 20 * 1024 * 1024 * 1024;
  gc-reserved-space = 256 * 1024 * 1024;
  substituters = [
    "https://cache.nixos.org"
    "https://codex-cli.cachix.org"
    "https://claude-code.cachix.org"
  ];
  trusted-public-keys = [
    "codex-cli.cachix.org-1:1Br3H1hHoRYG22n//cGKJOk3cQXgYobUel6O8DgSing="
    "claude-code.cachix.org-1:YeXf2aNu7UTX8Vwrze0za1WEDS+4DuI2kVeWEE4fsRk="
  ];
}
