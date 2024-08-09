rec {
  giteaSecret = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHRNHlKPGVnOATsbnkPccyK+C15TWGKbBwqNKt0UcQ81 imalison@adele";
  hostKeys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIG3UqIYs/NY0okKuiIO+dU2OM7A8vv3b6//GedagvLoX ryzen-shine.local"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINFbM1sL/vlDhrqPV1OMIGi4dKG0tMKhWSXx95ccbfyM biskcomp.local"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIoHW29TmS5FgK12N+bCXhGWASDdmzqSEA0QxbyGaJ+j nixquick.local"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIaA0tRVTqwBDxY6X03wx+50pbB37y5e8gqFpFMDa/Bj adele.local"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAID4j70BoksIEiX+OMBCsSG8wvMIEwoRQf5Gz5ppbm7Iy jimi-hendnix.local"
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
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICJ7S93cC2gsu/JbnDsa6X1omJzn1mI6ppKd2k+yV4aD imalison@jimi-hendnix"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIiZd2FiyTJvuvDh5hH0L3BqZV3E/kwwyau57QD7pz7C cardno:000614590850" # Dfinity Admin
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHOEt0T+Hxxat5tbkD9mSu8T271QjRrLr2EA0rIDXUNL cardno:000614590748" # Dfinity Read-Only
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMCJ08qswd3OoApAIHQwojEUJ4sre89vSngbM3x5pBP2 imalison@jay-lenovo.local" # Kat's Lenovo Legion
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOVGIGnpkU7HNQ/zl/Ffi562M+laWY9/yIjB63BCMiTS kat@nixcomp.local"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIO3tlMePru6ZlSuf8yUii3N1dy3WwJnSQAt3EgETkctK kat@jay-lenovo.local"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFrOYD3ReFc2+xFUylBFHREcm1lO7BRJGW5JrOoY3I8s ivanm@strixi-minaj"
  ];
  deanKeys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICDvbEVL+y7eV4+mtxOuHwyomBBQ6uYMesctstua20+e deanwenstrand@deans-mbp-2.lan"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFeXvPyHIYBPtn6QJtOrjlBUsZjDonVRfmWqTTM2ITWx nixos@nixos"
  ];
  alexKeys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIP2SQkJenAX67Ze99SKOVpKDD1XvAZnxQ8RLP0dL/Ej2 alexm@MALISONSERVER"
  ];
  mikeKeys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPnXd6c9xwr1yxBmxauj/FF3gnY8G11ospoM8i11mD2n countablecloud.com"
  ];
  andyKeys = [
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCqBnDZXcZIMtOTPxg4pJWyMyNp84kpU2uH88aCkrhMBY2TI4dqAXGY4h1DOyOEyQo9nTgyvdlv4M1Hlp5tzfbuapdTWtt36gb+FKpsTUy8dz45fhKJerUszNbVQ+SpZu4ZFN4Nv9eP3AHO+Mzjcvbf666VMDEzZWu2totd7jxyI9PdK78TD5AOi+kGTtbEgqMpJCcZmJlzJ9NJJ2ejhPCRwcfDrShHcfMeEwWL5MmnzT+WstkRwJAM0f9/ks77jWDnB1Eu55rQykU8zVZ1nz1GiXZcxUpm6B0IRhwn8NGH6nVzSLITtO5eJ7LPQR9XNUODpbrsVGNK6jgGql9gZpYp andy@nixos"
  ];
  willKeys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILgLjW6R+dTEmlUoPfE4GA8gRn93UlcSJqlJP1xw0KWl willgester@gmail.com"
  ];
  loewyKeys = [
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDROb8zcXAgqR3xNpStjt8kSL2Tnic+aWVlQRkmmLveay0RDPatHVKiWtscBoFYvL19xwpi692nZjyPAGQBLMquVx8rexHUFVbs6UVM41Y/QV0UZLSlVM7xNl3nL/dQoxT8OC2a9WJThNm41EjFzzKAuUaWqjm4+uEsC9felBIzndlA5/bIn6EUkMb2X8qmOOIOod1UeVZeK0fWMoDdKsHtQjiQrrP4nLjOmrTQ+BF2yUHwFbW6SCQiXT1Jzq4zymnI717ZraTK0nXzl8amLrGGrh36TrR7pv9hWLeNIMCARvOtABMdQmrT1dI4FxLK1uKM696uzfoaZDUn58G2VGrd loewy gitlab"
  ];
  agenixKeys = hostKeys ++ kanivanKeys;
  allKeys = loewyKeys ++ mikeKeys ++ kanivanKeys ++ deanKeys ++ alexKeys ++ hostKeys;
  railbird-sf = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDozY/3Cd9npaYPCgIn/E7MjW9c7Zb5/wTO5Qi7yRU45 root@railbird-sf"
  ];
  micahKeys = [
    "AAAAB3NzaC1yc2EAAAADAQABAAACAQC4ayU9sly58EHQ2YdrnoJ5o0mFD8I8UiDvN3kVh6oSMMQ9wkVzLUY6wUzlc0uq3P/OYco9mURMt83C+lY4UC+jvN09nIEHV7yVJWsKV3ntmnEqM63e9cOWnpC9XA1MSha7xrf9DaO/doxST4fY6ixZA+nbE0bFgNTqyjzdya7pCamAdnPqKqwUEha+0KfpA8TJlrfgrDLd65f2/+jLU8fhTkU6yhrJ9Piq7C+RwVTu/dlrp50PNirfQRSa7vUPNS8Y973XtK6jzB/xS2Qc6iGJ+4QcCy6lgu2xksO/MNIimZYGOCP2yx6/GDp/gnOlAktrJ3flMycJdEHkZVH61HfQV76O/mY3tck/SiCwhuNxXw0uh9wf4UO8JHlnJcsIsBkSBemJsxQul+G7kV4EV/gqW/iO5U2nKmDqbwNWigO9mN44LqBOwsnAnu78kiSE8sNCEdSy/qVWloBw4VyT/emvmTwP1okh/CKNnu0/HmfSNL6Vib1XuvKSPvrUIOLMrSeRdtu85LrWaS0CvKEoMrIgpvc+3jHtdbxwINE3JBGrbXvfe20CuHl6Fx0ONkhirRmrjG7iUjV/4c6nS2aguPVsGi7q3GYvtz7cNEe3XbfpZaHzrCncnoQPbnSDLjHdeZf4O5Aew6iOwwkpgBBDA3lJO55ujq2vNbgkpH3hmQjltQ=="
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBEBce6E/XCdo8+dC4PPyyTsO/Z0BZxuz0IrTLaGehrT weitzman.micah@gmail.com"
  ];
  benKeys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAJ6lD0c+frh2vzQjvsrsmJpwM1ovaY59m5NNPml5G+E benjamin.j.corner@gmail.com"
  ];
  interviewKeys = [
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDr3Rim92K0aNWh2wCZjZMj927H6NZMvw+Jg5giVEklv/eeXlJn81xm9kFsqWOmN0IyS4X+2V2G/43t4C+REmzMKjkzTu9u6SJi+pC3jdJRm80sJCz2LBdRjPaY3CEubiUdxWvjisE8wUI1YuxxdegJpcemvhZXBfDU+DOqNEtF9KiPhXFMOoGU18k5AqFatdmE4z0H158cc6rCxdEiM8s+H6AJDA+xZSn9BPSx2I/ySRapoN0qJDloV19795ggnegBOjNuGejnKI0RZ8CyTcf1945Yi8/O/i/IChRlxiArjgAbC2DItkQig8YyQYz6WlKE9bwruMqe+8mgBAfuQXH8yTADwD1156VCRGoYAorAz4CLm73oGKtx4pa6WFsklWAIK1UEAhuLT6FKaJ/mmfKxcqdHxj+29FNo3d2Pam/ecFd0aERi5TExH5GKZr+LI7yKQmgk6laiR0n1wZuDwlNN7+69GH+ScxRngGCkc3RdTToqXzEaN3B6EyiAN/sAUgs= finn@LAPTOP-JI7BSSRJ"
  ];
}
