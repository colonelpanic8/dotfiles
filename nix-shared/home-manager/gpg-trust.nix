{lib, ...}: {
  programs.gpg = {
    enable = true;
    # Leave gpg.conf unmanaged; this module only pins the key and its trust.
    settings = lib.mkForce {};
    publicKeys = [
      {
        source = ../gpg/imalison-2024.asc;
        trust = "ultimate";
      }
    ];
  };
}
