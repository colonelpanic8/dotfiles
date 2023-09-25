let keys = (import ../keys.nix);
in
{
  "gpg-keys.age".publicKeys = keys.agenixKeys;
  "gpg-passphrase.age".publicKeys = keys.agenixKeys;
  "cache-priv-key.pem.age".publicKeys = keys.agenixKeys;
  "gitea-runner-token.age".publicKeys = keys.agenixKeys;
  "gitea-runner-token.ryzen-shine.age".publicKeys = keys.agenixKeys;
}
