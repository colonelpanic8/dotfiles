# Run from the repository root (runtime-only checks need only nixpkgs):
# nix eval --json --impure --expr 'import ./nixos/checks/tailscale-module.nix { nixpkgs = (builtins.getFlake (toString ./nix-darwin)).inputs.nixpkgs; }'
{
  nixpkgs,
  agenix ? null,
  darwin ? null,
}: let
  lib = nixpkgs.lib;
  evaluate = modules:
    nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules =
        [
          ../tailscale.nix
          {
            boot.isContainer = true;
            system.stateVersion = "26.05";
          }
        ]
        ++ modules;
    };
  disabled = evaluate [
    {
      myModules.tailscale = {
        enable = false;
        enrollment = "unused";
        enrollmentProfiles.unused.authKeyFile = throw "disabled enrollment must not evaluate its credential";
      };
    }
  ];
  manual = evaluate [
    {
      myModules.tailscale.enrollmentProfiles.unused.authKeyFile = throw "manual authentication must not evaluate credentials";
    }
  ];
  alex = evaluate [{myModules.tailscale.enrollment = "alex";}];
  custom = evaluate [
    {
      networking.hostName = "third-pc";
      myModules.tailscale = {
        enrollment = "third-tailnet";
        enrollmentProfiles = {
          third-tailnet = {
            authKeyFile = "/nonexistent-unread-runtime-secret";
            operator = "another-user";
            acceptDns = false;
            advertiseTags = ["tag:third-pc"];
          };
          unused.authKeyFile = throw "unselected credentials must stay unevaluated";
        };
      };
    }
  ];
  unknown = evaluate [{myModules.tailscale.enrollment = "typo";}];
  noCredential = evaluate [
    {
      myModules.tailscale = {
        enrollment = "empty";
        enrollmentProfiles.empty = {};
      };
    }
  ];
  bothCredentials = evaluate [
    {
      myModules.tailscale.enrollment = "alex";
      myModules.tailscale.enrollmentProfiles.alex.authKeySecret = ../secrets/tailscale-authkey.age;
    }
  ];
  unsafe = path:
    evaluate [
      {
        myModules.tailscale.enrollment = "alex";
        myModules.tailscale.enrollmentProfiles.alex.authKeyFile = path;
      }
    ];
  conflicting = evaluate [
    {
      myModules.tailscale.enrollment = "alex";
      services.tailscale.authKeyFile = "/other-credential";
    }
  ];
  missingAgenix = evaluate [{myModules.tailscale.enrollment = "ivan";}];
  allAssertionsPass = system: lib.all (item: item.assertion) system.config.assertions;
  hasEnrollmentUnit = system: system.config.systemd.services ? tailscale-autoconnect;
  env = system: system.config.myModules.tailscale.enrollmentEnvironment;
  encrypted = evaluate [agenix.nixosModules.default {myModules.tailscale.enrollment = "ivan";}];
  encryptedAlex = evaluate [agenix.nixosModules.default {myModules.tailscale.enrollment = "alex";}];
  encryptedManual = evaluate [agenix.nixosModules.default {}];
  darwinEval = settings:
    darwin.lib.darwinSystem {
      modules = [
        ../../nix-darwin/tailscale.nix
        agenix.darwinModules.default
        {
          nixpkgs.hostPlatform = "aarch64-darwin";
          system.primaryUser = "kat";
          system.stateVersion = 7;
          networking.hostName = "darwin-test";
          age.identityPaths = ["/etc/ssh/ssh_host_ed25519_key"];
        }
        settings
      ];
    };
  darwinKat = darwinEval {myModules.tailscale.enrollment = "kat";};
  darwinManual = darwinEval {};
  darwinDisabled = darwinEval {myModules.tailscale.enable = false;};
  tests =
    {
      disabledHasNoDaemon = !disabled.config.services.tailscale.enable;
      disabledHasNoEnrollmentUnit = !hasEnrollmentUnit disabled;
      manualHasNoEnrollmentUnit = !hasEnrollmentUnit manual;
      manualEnablesDaemon = manual.config.services.tailscale.enable;
      manualHasNoEnvironment = env manual == {};
      alexEvaluatesWithoutAgenix = allAssertionsPass alex && !(alex.config ? age);
      alexHasEnrollmentUnit = hasEnrollmentUnit alex;
      alexUsesRuntimePath = (env alex).AUTH_KEY_FILE == "/var/lib/tailscale-alex/authkey";
      alexPreservesExistingLogin = (env alex).REAUTHENTICATE_EXPIRED == "false";
      thirdProfileEvaluatesWithoutReadingSecret = allAssertionsPass custom;
      thirdProfileHasEnrollmentUnit = hasEnrollmentUnit custom;
      thirdProfileUsesOwnSettings =
        (env custom).AUTH_KEY_FILE
        == "/nonexistent-unread-runtime-secret"
        && (env custom).OPERATOR == "another-user"
        && (env custom).NODE_HOSTNAME == "third-pc"
        && (env custom).ACCEPT_DNS == "false"
        && (env custom).ADVERTISE_TAGS == "tag:third-pc";
      existingStateDirectoryIsUnchanged = !(alex.config.systemd.services.tailscaled.serviceConfig ? StateDirectory);
      upstreamEnrollmentIsAbsent = alex.config.services.tailscale.authKeyFile == null;
      unknownProfileIsRejected = !allAssertionsPass unknown;
      missingCredentialIsRejected = !allAssertionsPass noCredential;
      twoCredentialSourcesAreRejected = !allAssertionsPass bothCredentials;
      storePathIsRejected = !allAssertionsPass (unsafe "/nix/store/unsafe-secret");
      storeRootIsRejected = !allAssertionsPass (unsafe "/nix/store");
      relativePathIsRejected = !allAssertionsPass (unsafe "relative/authkey");
      upstreamEnrollmentIsRejected = !allAssertionsPass conflicting;
      missingAgenixIsRejected = !allAssertionsPass missingAgenix;
    }
    // lib.optionalAttrs (agenix != null) {
      encryptedProfileEvaluates = allAssertionsPass encrypted;
      encryptedProfileProvisionsOneSecret = builtins.attrNames encrypted.config.age.secrets == ["tailscale-authkey"];
      encryptedProfileUsesSelectedSecret = encrypted.config.age.secrets.tailscale-authkey.file == ../secrets/tailscale-authkey.age;
      encryptedProfileUsesDecryptedPath = (env encrypted).AUTH_KEY_FILE == encrypted.config.age.secrets.tailscale-authkey.path;
      encryptedProfileKeepsExpiryRepair = (env encrypted).REAUTHENTICATE_EXPIRED == "true";
      encryptedProfileIncludesIdentity = builtins.elem "/home/imalison/.ssh/id_ed25519" encrypted.config.age.identityPaths;
      runtimeProfileDoesNotProvisionSecrets = encryptedAlex.config.age.secrets == {};
      runtimeProfileDoesNotAddIvanIdentity = !(builtins.elem "/home/imalison/.ssh/id_ed25519" encryptedAlex.config.age.identityPaths);
      manualDoesNotProvisionSecrets = encryptedManual.config.age.secrets == {};
    }
    // lib.optionalAttrs (darwin != null && agenix != null) {
      darwinSelectedProfileEvaluates = allAssertionsPass darwinKat;
      darwinSelectedProfileProvisionsOneSecret = builtins.attrNames darwinKat.config.age.secrets == ["tailscale-authkey"];
      darwinUsesSameEnrollmentEnvironment = darwinKat.config.launchd.daemons.tailscale-enroll.serviceConfig.EnvironmentVariables == env darwinKat;
      darwinUsesSelectedOperator = (env darwinKat).OPERATOR == "kat";
      darwinUsesSelectedSecret = darwinKat.config.age.secrets.tailscale-authkey.file == ../secrets/tailscale-authkey.kats-macbook-neo.age;
      darwinManualHasNoEnrollmentJob = !(darwinManual.config.launchd.daemons ? tailscale-enroll);
      darwinManualHasNoSecret = darwinManual.config.age.secrets == {};
      darwinManualStillReconcilesPreferences = darwinManual.config.launchd.daemons ? tailscale-autoconnect;
      darwinDisabledHasNoJobs =
        !(darwinDisabled.config.launchd.daemons ? tailscale-enroll)
        && !(darwinDisabled.config.launchd.daemons ? tailscale-autoconnect);
    };
in
  assert lib.assertMsg (lib.all (passed: passed) (builtins.attrValues tests)) (builtins.toJSON tests); tests
