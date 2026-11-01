{
  hosts = [
    "ryzen-shine"
    "strixi-minaj"
    "jay-lenovo"
    "jimi-hendnix"
    "mac-demarco-mini"
    "railbird-sf"
  ];
  sshTargets = {
    # The active macOS account is still `kat`; switch this back to the bare
    # hostname when the primary-account migration to `imalison` is complete.
    mac-demarco-mini = "kat@mac-demarco-mini";
  };
}
