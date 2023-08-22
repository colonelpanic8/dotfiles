let keys = (import ../keys.nix);
in
{
  "gpg-keys.age".publicKeys = keys.kanivanKeys;
}
