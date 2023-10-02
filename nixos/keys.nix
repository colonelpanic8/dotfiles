rec {
  hostKeys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIG3UqIYs/NY0okKuiIO+dU2OM7A8vv3b6//GedagvLoX ryzen-shine.local"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINFbM1sL/vlDhrqPV1OMIGi4dKG0tMKhWSXx95ccbfyM biskcomp.local"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIoHW29TmS5FgK12N+bCXhGWASDdmzqSEA0QxbyGaJ+j nixquick.local"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIaA0tRVTqwBDxY6X03wx+50pbB37y5e8gqFpFMDa/Bj adele.local"
  ];
  kanivanKeys = [
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDUSkj7587e+MAUNyU/KRpw9Vk++53Wv5nB+0V1QgiTO3rMQe6HJt0Tm2wi/o/T8GNjueT2D69YgkqOIF1FQwsj2EFLObcMzeBgs5gTSglqggA2I91BIc1vvgjCDpogOMAzAQGlTxRnqrEXhqG0jJtw8KIzLr9WrvWLdTT4rHtWS8RoOBgkQ8oxbggZ4vtbMBIwoIAYGRr70KBRNCsLTPLa8yEf+DDQxq1entzxSjHXHgyeBSVVpPCrBVmhjandk+lIFInjvAiAE1ZkJHSRccL73ORmgb1crwH7xlD9NwBPmypowMi8UIRMKfL2lNehT0AQIlEAikUBLMDzPIPhnwLZ imalison@ivanm-dfinity-razer.local"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHEsLV27EteTsuVl1gLAZRCklpMFBMhakKbQ2+MkN5rm JuiceSSH"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICzGkqGJm+nrMvsrfuWOLVxXHvi0UL1ULJmyfzS9sKpy imalison@biskcomp"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOuO/tc728fKyctlufiehZQuKsD0XDiS/5x7TImk0Ip4 imalison@ivanm-dfinity-razer"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDt/rcYuGGlXBcRUJvzUCgOW8PNVkJJ5TwEOha1/KGM4 imalison@stevie-nixos"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICzGkqGJm+nrMvsrfuWOLVxXHvi0UL1ULJmyfzS9sKpy imalison@biskcomp"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJr9kVlYIZIPXfXom4Fi7S2yvp5sWJ6BSM5m3uLh+8y5 imalison@adele"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOOFkA5JZkq8mRd7St0jP2P6WyYYhW2CChmQoY20N45f imalison@ryzen-shine"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIiZd2FiyTJvuvDh5hH0L3BqZV3E/kwwyau57QD7pz7C cardno:000614590850" # Dfinity Admin
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHOEt0T+Hxxat5tbkD9mSu8T271QjRrLr2EA0rIDXUNL cardno:000614590748" # Dfinity Read-Only
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMCJ08qswd3OoApAIHQwojEUJ4sre89vSngbM3x5pBP2 imalison@jay-lenovo.local" # Kat's Lenovo Legion
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOVGIGnpkU7HNQ/zl/Ffi562M+laWY9/yIjB63BCMiTS kat@nixcomp.local"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIO3tlMePru6ZlSuf8yUii3N1dy3WwJnSQAt3EgETkctK kat@jay-lenovo.local"
  ];
  deanKeys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICDvbEVL+y7eV4+mtxOuHwyomBBQ6uYMesctstua20+e deanwenstrand@deans-mbp-2.lan"
  ];
  alexKeys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIP2SQkJenAX67Ze99SKOVpKDD1XvAZnxQ8RLP0dL/Ej2 alexm@MALISONSERVER"
  ];
  agenixKeys = hostKeys ++ kanivanKeys;
  allKeys = kanivanKeys ++ deanKeys ++ alexKeys ++ hostKeys;
}
