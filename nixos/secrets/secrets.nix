let keys = (import ../keys.nix);
in
{
  "gpg-keys.age".publicKeys = keys.agenixKeys;
  "cache-priv-key.pem.age".publicKeys = keys.agenixKeys;
}
