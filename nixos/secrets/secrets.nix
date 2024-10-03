let keys = (import ../keys.nix);
in
{
  "gpg-keys.age".publicKeys = keys.agenixKeys;
  "gpg-passphrase.age".publicKeys = keys.agenixKeys;
  "cache-priv-key.pem.age".publicKeys = keys.agenixKeys;
  "gitea-runner-token.biskcomp.age".publicKeys = keys.agenixKeys;
  "gitea-runner-token.ryzen-shine.age".publicKeys = keys.agenixKeys;
  "gitea-runner-token.nixquick.age".publicKeys = keys.agenixKeys;
  "gitea-runner-token.jimi-hendnix.age".publicKeys = keys.agenixKeys;
  "gitea-runner-token.adele.age".publicKeys = keys.agenixKeys;
  "gitea-runner-token.railbird-sf.age".publicKeys = keys.agenixKeys ++ keys.railbird-sf;
  "gitea-runner-token.mac-demarco-mini.age".publicKeys = keys.agenixKeys ++ keys.railbird-sf;
  "gitea-runner-token.age".publicKeys = keys.agenixKeys ++ keys.railbird-sf;
  "nextcloud-admin.age".publicKeys = keys.agenixKeys;
  "ryzen-shine-kubernetes-token.age".publicKeys = keys.agenixKeys;
  "1896Folsom-k3s-token.age".publicKeys = keys.agenixKeys ++ keys.railbird-sf;
  "api_service_account_key.json.age".publicKeys = keys.agenixKeys;
  "k3s-registry.yaml.age".publicKeys = keys.agenixKeys ++ keys.railbird-sf;
}
