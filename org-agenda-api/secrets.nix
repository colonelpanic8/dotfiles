let
  keys = import ../nixos/keys.nix;
in
{
  # colonelpanic instance
  "configs/colonelpanic/secrets/git-ssh-key.age".publicKeys = keys.kanivanKeys;
  "configs/colonelpanic/secrets/auth-password.age".publicKeys = keys.kanivanKeys;

  # kat instance
  "configs/kat/secrets/git-ssh-key.age".publicKeys = keys.kanivanKeys;
  "configs/kat/secrets/auth-password.age".publicKeys = keys.kanivanKeys;
}
