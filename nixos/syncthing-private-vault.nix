{
  config,
  lib,
  pkgs,
  ...
}: let
  syncDir = "/var/lib/syncthing/sync";
  cipherName = "Private.encrypted";
  cipherDir = "${syncDir}/${cipherName}";
  runtimeDir = "/run/syncthing-private-vault";
  mountDir = "${runtimeDir}/mnt";
  passFile = "${runtimeDir}/passphrase";
  linkPath = "/home/imalison/Private";
  # Only imalison and the syncthing service user; the shared syncthing group
  # includes every local user.
  vaultGroup = "syncthing-private-vault";

  # Syncthing must never send or accept a name under the cipher dir that
  # gocryptfs could not have produced: ciphertext names are base64url and at
  # least 22 characters long; metadata files start with "gocryptfs.".
  # The glob library allows only one range or plain list per character class.
  base64urlChars = "-ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_";
  shortNamePatterns = n: let
    name = lib.concatStrings (lib.genList (_: "?") n);
  in [
    "/${cipherName}/${name}"
    "/${cipherName}/**/${name}"
    "/${cipherName}/${name}/**"
    "/${cipherName}/**/${name}/**"
  ];
  stignoreBlock = pkgs.writeText "syncthing-private-vault.stignore" (lib.concatLines (
    [
      "// BEGIN syncthing-private-vault (managed by NixOS)"
      "!/${cipherName}/gocryptfs.*"
      "!/${cipherName}/**/gocryptfs.*"
      "/${cipherName}/**[!${base64urlChars}/]**"
    ]
    ++ lib.concatMap shortNamePatterns (lib.range 1 21)
    ++ ["// END syncthing-private-vault"]
  ));
in
  lib.mkIf config.myModules.syncthing.enable {
    system.activationScripts.syncthingPrivateVault = {
      deps = ["syncthingPermissions"];
      # Only the syncthing user (the daemon and the gocryptfs mount) may write
      # ciphertext, so nothing local can drop plaintext into it. The group keeps
      # read access because the mount's root inherits the cipher dir's mode.
      text = ''
        install -d -o syncthing -g syncthing ${cipherDir}
        chmod -R g-w,o= ${cipherDir}
        chgrp ${vaultGroup} ${cipherDir}

        stignore=${syncDir}/.stignore
        stignore_new="$(mktemp)"
        cat ${stignoreBlock} > "$stignore_new"
        if [ -f "$stignore" ]; then
          ${pkgs.gnused}/bin/sed '/^\/\/ BEGIN syncthing-private-vault/,/^\/\/ END syncthing-private-vault/d' "$stignore" >> "$stignore_new"
        fi
        if ! ${pkgs.diffutils}/bin/cmp -s "$stignore_new" "$stignore"; then
          install -o syncthing -g syncthing -m 0660 "$stignore_new" "$stignore"
        fi
        rm -f "$stignore_new"
      '';
    };

    users.groups.${vaultGroup} = {};
    users.users.imalison.extraGroups = [vaultGroup];
    users.users.syncthing.extraGroups = [vaultGroup];

    # The mount runs as syncthing so every ciphertext file it writes stays
    # writable by the syncthing daemon; force_owner presents it as imalison.
    programs.fuse.userAllowOther = true;

    systemd.tmpfiles.rules = [
      "d ${runtimeDir} 2750 imalison ${vaultGroup} -"
      "d ${mountDir} 0750 syncthing ${vaultGroup} -"
    ];

    systemd.services.syncthing-private-vault = {
      description = "Decrypted view of the syncthing private vault";
      unitConfig.ConditionPathExists = "${cipherDir}/gocryptfs.conf";
      path = ["/run/wrappers" pkgs.gocryptfs pkgs.coreutils pkgs.util-linux pkgs.getent];
      script = ''
        exec gocryptfs -q -sharedstorage -allow_other \
          -force_owner "$(id -u imalison):$(getent group ${vaultGroup} | cut -d: -f3)" \
          -passfile ${passFile} ${cipherDir} ${mountDir}
      '';
      serviceConfig = {
        Type = "forking";
        User = "syncthing";
        Group = "syncthing";
        ExecStartPost = "+${pkgs.coreutils}/bin/rm -f ${passFile}";
        ExecStopPost = [
          "+${pkgs.coreutils}/bin/rm -f ${passFile}"
          "-+${pkgs.util-linux}/bin/umount -l ${mountDir}"
        ];
      };
    };

    home-manager.users.imalison = {lib, ...}: {
      home.packages = [pkgs.gocryptfs];

      home.activation.linkSyncthingPrivateVault = lib.hm.dag.entryAfter ["writeBoundary"] ''
        if [ -d ${linkPath} ] && [ ! -L ${linkPath} ]; then
          rmdir ${linkPath} || echo "warning: ${linkPath} is a non-empty directory; not linking the vault" >&2
        fi
        if [ ! -e ${linkPath} ] || [ -L ${linkPath} ]; then
          ln -sfn ${mountDir} ${linkPath}
        fi
      '';
    };
  }
