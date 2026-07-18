{
  pkgs,
  config,
  inputs,
  lib,
  makeEnable,
  ...
}: let
  codexSharedAppServerUrl = "ws://127.0.0.1:46231";
  claudeDesktopSource = inputs.claude-desktop;
  claudeDesktop = pkgs.callPackage "${claudeDesktopSource}/nix/claude-desktop.nix" {};
  claudeDesktopFhs = pkgs.callPackage "${claudeDesktopSource}/nix/fhs.nix" {
    claude-desktop = claudeDesktop;
  };
  codexDesktopLinux = let
    flake = import "${inputs.codex-desktop-linux}/flake.nix";
    self' =
      (flake.outputs {
        self = self';
        nixpkgs = inputs.nixpkgs;
        flake-utils = inputs.flake-utils;
      })
      // {
        outPath = "${inputs.codex-desktop-linux}";
        rev = inputs.codex-desktop-linux.rev or "";
        lastModified = inputs.codex-desktop-linux.lastModified or 1;
      };
  in
    self';
  codexDesktopLinuxPackage = let
    package =
      codexDesktopLinux.packages.${pkgs.stdenv.hostPlatform.system}.codex-desktop-computer-use-ui-remote-mobile-control;
    gsettingsSchemaDataDirs = lib.concatMapStringsSep ":" (pkg:
      lib.removeSuffix "/glib-2.0/schemas" (pkgs.glib.getSchemaPath pkg)) (with pkgs; [
      gsettings-desktop-schemas
      gtk3
    ]);
  in
    package.overrideAttrs (oldAttrs: {
      nativeBuildInputs = (oldAttrs.nativeBuildInputs or []) ++ [pkgs.asar];
      src = oldAttrs.src.overrideAttrs (payloadOldAttrs: {
        installPhase = ''
          export CODEX_ENFORCE_CRITICAL_PATCHES=0
          ${payloadOldAttrs.installPhase}
        '';
      });
      # Keep this outside the upstream source so input updates cannot cause
      # source-patch conflicts. This can go away once the package wrapper adds
      # the GSettings schema roots itself upstream.
      postFixup =
        (oldAttrs.postFixup or "")
        + ''
          # The upstream Desktop bundle normally constructs its local host
          # without a websocket URL and therefore owns a private app-server
          # child process. Supplying websocket_url selects its existing client
          # transport instead, so Desktop and the CLI can share the durable
          # systemd-managed app-server below.
          app_asar="$out/opt/codex-desktop/resources/app.asar"
          app_asar_dir="$(mktemp -d)"
          ${lib.getExe pkgs.asar} extract "$app_asar" "$app_asar_dir"
          main_bundle="$(find "$app_asar_dir/.vite/build" -name 'main-*.js' -print -quit)"
          perl -0pi -e '
            BEGIN { $count = 0 }
            $count += s/\{id:([A-Za-z_\$][A-Za-z0-9_\$]*),display_name:`Local`,kind:`local`\}/\{id:$1,display_name:`Local`,kind:`local`,websocket_url:process.env.CODEX_APP_SERVER_WS_URL??null\}/g;
            END { die "expected exactly one local Codex host object, found $count\n" unless $count == 1 }
          ' "$main_bundle"
          ${lib.getExe pkgs.asar} pack "$app_asar_dir" "$app_asar.new"
          mv "$app_asar.new" "$app_asar"
          rm -r "$app_asar_dir"

          wrapProgram "$out/bin/codex-desktop" \
            --set CODEX_APP_SERVER_WS_URL "${codexSharedAppServerUrl}" \
            --prefix XDG_DATA_DIRS : "${gsettingsSchemaDataDirs}"
        '';
    });
in
  makeEnable config "myModules.code" true {
    programs.direnv = {
      enable = true;
      nix-direnv.enable = true;
    };

    # Claude Desktop downloads its own generic-linux Claude Code build to
    # ~/.config/Claude/claude-code/<version>/claude and execs it for every
    # session. That binary is dynamically linked against /lib64/ld-linux-x86-64.so.2,
    # which on stock NixOS is stub-ld, so it dies with exit 127. The FHS variant
    # used to supply a loader inside its bwrap sandbox; running claude-desktop
    # non-FHS (so the integrated terminal can sudo) takes that away. nix-ld puts a
    # real loader back at the FHS path, for that binary and any other prebuilt
    # native tooling (MCP servers, Cowork) these apps fetch at runtime.
    programs.nix-ld.enable = true;

    hardware.uinput.enable = lib.mkIf config.myModules.desktop.enable true;

    programs.ydotool = lib.mkIf config.myModules.desktop.enable {
      enable = true;
      group = "input";
    };

    home-manager.sharedModules = lib.mkIf config.myModules.desktop.enable [
      codexDesktopLinux.homeManagerModules.default
      {
        home.sessionVariables.YDOTOOL_SOCKET = "/run/ydotoold/socket";
        systemd.user.sessionVariables.YDOTOOL_SOCKET = "/run/ydotoold/socket";

        xdg.configFile."codex-desktop/settings.json".text =
          (builtins.toJSON {
            "codex-linux-computer-use-ui-enabled" = true;
          })
          + "\n";
      }
    ];

    home-manager.users.imalison = lib.mkIf config.myModules.desktop.enable {
      programs.codex = {
        enable = true;
        package = pkgs.codex;
      };

      programs.codexDesktopLinux = {
        enable = true;
        package = codexDesktopLinuxPackage;
        # Bake CODEX_CLI_PATH into the launcher so Codex Desktop always finds this
        # CLI, regardless of how it is started (GUI autostart, app launcher,
        # terminal, or warm-start handoff) and without needing a re-login.
        cliPackage = pkgs.codex;
        computerUseUi.enable = true;
        remoteMobileControl.enable = true;
        remoteControl = {
          enable = true;
          package = pkgs.codex;
          extraPackages = with pkgs; [
            bash
            coreutils
            findutils
            git
            gnugrep
            gnused
            nix
            nodejs
            openssh
            ripgrep
            zsh
          ];
          # Loopback WebSocket is understood by both the Desktop client path
          # above and `codex --remote`; unlike the Desktop-owned stdio child,
          # this process survives when the UI exits.
          listen = codexSharedAppServerUrl;
        };
      };
    };

    environment.systemPackages = with pkgs;
      [
        # LLM Tools
        # antigravity
        claude-code
        codex
        gemini-cli
        inputs.gmcli.packages.${pkgs.stdenv.hostPlatform.system}.default
        inputs.lastfm-edit.packages.${pkgs.stdenv.hostPlatform.system}.scrobble-scrubber-app
        opencode
        t3code

        # MCP
        github-mcp-server
        gitea-mcp-server
        gws
        playwright-mcp
        playwright-cli

        # C
        clang

        # Haskell
        cabal-install
        ghc
        haskellPackages.hpack
        haskellPackages.hasktags
        haskellPackages.hoogle

        # Scala
        sbt
        scala

        # Node
        nodejs
        yarn
        prettier

        # Typescript
        typescript
        typescript-language-server

        # golang
        go

        # Rust
        rustup
        cargo-sweep

        # Clojure
        boot
        leiningen

        # Ruby
        ruby

        # python
        black
        poetry
        uv

        # kotlin
        kotlin
        kotlin-language-server

        # dhall
        haskellPackages.dhall
        haskellPackages.dhall-json

        # misc
        perf-tools
        protobuf

        # nix
        nixd
        nil
        alejandra
      ]
      ++ lib.optionals (config.networking.hostName != "jay-lenovo") [
        # Non-FHS variant: runs in the host namespace (no bwrap userns), so the
        # integrated Claude Code terminal can use sudo / nixos-rebuild. The FHS
        # variant (claudeDesktopFhs) sandboxes everything in an unprivileged user
        # namespace, which makes host root impossible. The FHS layout the app's
        # downloaded native binaries expect is supplied by nix-ld above instead.
        claudeDesktop
        cabal2nix
      ]
      ++ (
        if pkgs.stdenv.hostPlatform.system == "x86_64-linux"
        then
          with pkgs; [
            # purescript
            purescript
            # Broken
            # spago
          ]
        else []
      );
  }
