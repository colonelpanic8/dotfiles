{
  inputs = {
    nixpkgs = {
      url = "github:NixOS/nixpkgs/nixos-unstable";
    };

    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.systems.follows = "systems";
    };

    systems = {url = "github:nix-systems/default";};

    git-ignore-nix = {
      url = "github:hercules-ci/gitignore.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixos-hardware = {url = "github:colonelpanic8/nixos-hardware/my-master";};

    nixos-wsl = {url = "github:nix-community/NixOS-WSL";};

    agenix = {url = "github:ryantm/agenix";};

    railbird-secrets = {
      url = "git+ssh://gitea@dev.railbird.ai:1123/railbird/secrets-flake.git";
    };
    # railbird-secrets = {
    #   url = "git+ssh://gitea@dev.railbird.ai:1123/railbird/secrets-flake.git";
    # };

    xmonad = {
      url = "github:xmonad/xmonad";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
        git-ignore-nix.follows = "git-ignore-nix";
      };
    };

    xmonad-contrib = {
      url = "github:IvanMalison/xmonad-contrib/withMyChanges";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
        git-ignore-nix.follows = "git-ignore-nix";
        xmonad.follows = "xmonad";
      };
    };

    taffybar = {
      url = "github:taffybar/taffybar/old-master";
      inputs = {
        nixpkgs.follows = "nixpkgs";
      };
    };

    imalison-taffybar = {
      url = "path:../dotfiles/config/taffybar";
      # inputs = {
      #   nixpkgs.follows = "nixpkgs";
      #   flake-utils.follows = "flake-utils";
      #   xmonad.follows = "xmonad";
      #   taffybar.follows = "taffybar";
      # };
    };

    notifications-tray-icon = {
      url = "github:IvanMalison/notifications-tray-icon";
      inputs.flake-utils.follows = "flake-utils";
      inputs.git-ignore-nix.follows = "git-ignore-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    gtk-sni-tray = {
      url = "github:taffybar/gtk-sni-tray";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
        git-ignore-nix.follows = "git-ignore-nix";
        status-notifier-item.follows = "status-notifier-item";
      };
    };

    status-notifier-item = {
      url = "github:taffybar/status-notifier-item";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
        git-ignore-nix.follows = "git-ignore-nix";
      };
    };

    gtk-strut = {
      url = "github:taffybar/gtk-strut";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
        git-ignore-nix.follows = "git-ignore-nix";
      };
    };

    vscode-server.url = "github:nix-community/nixos-vscode-server";

    nixified-ai = {url = "github:nixified-ai/flake";};

    nixtheplanet.url = "github:matthewcroughan/nixtheplanet";

    org-agenda-api = {
      url = "github:colonelpanic8/org-agenda-api";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs @ {
    self,
    nixpkgs,
    nixos-hardware,
    home-manager,
    taffybar,
    xmonad,
    nixtheplanet,
    xmonad-contrib,
    notifications-tray-icon,
    nix,
    agenix,
    imalison-taffybar,
    ...
  }: let
    # Nixpkgs PR patches - just specify PR number and hash
    nixpkgsPRPatches = [
      {
        pr = 434160; # git-sync-rs package
        hash = "sha256-zjzjmC1XJmwfHr/YXFyYsqUFR5MHSoxWWyxIR35YNbM=";
      }
      # claude-code
      # {
      #   pr = 464698;
      #   hash = "sha256-Pe9G6b/rI0874mM7FIOSEKiaubk95NcFhTQ7paAeLTU=";
      # }
      # {
      #   pr = 464816;
      #   hash = "sha256-bKEoRy4dzP5TyRBjYskwEzr7tj8/ez/Y1XHiQgu5q5I=";
      # }
    ];

    # Custom patches that don't fit the PR template
    nixpkgsCustomPatches = [
    ];

    # Home-manager PR patches - just specify PR number and hash
    homeManagerPRPatches = [
      # Example:
      # {
      #   pr = 1234;
      #   hash = "sha256-...";
      # }
    ];

    # Custom home-manager patches that don't fit the PR template
    homeManagerCustomPatches = [
      {
        url = "https://github.com/colonelpanic8/home-manager/commit/92f4b7aa5254f8bcddc9ef86e04ea5314410d10b.patch";
        hash = "sha256-RQl5daVpCqQi05l9QfTEz2PpQxmsv/HYnXrgXbqbwWk=";
      }
    ];

    # Convert PR patches to full patch format for nixpkgs
    nixpkgsPrPatchesToPatches = prPatches:
      map (p: {
        url = "https://patch-diff.githubusercontent.com/raw/NixOS/nixpkgs/pull/${toString p.pr}.patch";
        hash = p.hash;
      })
      prPatches;

    # Convert PR patches to full patch format for home-manager
    homeManagerPrPatchesToPatches = prPatches:
      map (p: {
        url = "https://patch-diff.githubusercontent.com/raw/nix-community/home-manager/pull/${toString p.pr}.patch";
        hash = p.hash;
      })
      prPatches;

    # Combine all nixpkgs patches
    allNixpkgsPatches = (nixpkgsPrPatchesToPatches nixpkgsPRPatches) ++ nixpkgsCustomPatches;

    # Combine all home-manager patches
    allHomeManagerPatches = (homeManagerPrPatchesToPatches homeManagerPRPatches) ++ homeManagerCustomPatches;

    machinesFilepath = ./machines;
    machineFilenames = builtins.attrNames (builtins.readDir machinesFilepath);
    machineNameFromFilename = filename: builtins.head (builtins.split "\\." filename);
    machineNames = map machineNameFromFilename machineFilenames;
    mkConfigurationParams = filename: {
      name = machineNameFromFilename filename;
      value = {
        modules = [
          (machinesFilepath + ("/" + filename))
          agenix.nixosModules.default
          nixtheplanet.nixosModules.macos-ventura
          inputs.org-agenda-api.nixosModules.default
        ];
      };
    };
    defaultConfigurationParams =
      builtins.listToAttrs (map mkConfigurationParams machineFilenames);
    customParams = {
      biskcomp = {
        system = "aarch64-linux";
      };
      air-gapped-pi = {
        system = "aarch64-linux";
      };
    };
    mkConfig = {
      system ? "x86_64-linux",
      baseModules ? [],
      modules ? [],
      specialArgs ? {},
      ...
    }: let
      # Bootstrap nixpkgs for this specific system
      bootstrapPkgs = import nixpkgs {
        inherit system;
        config = {};
        overlays = [];
      };
      # Apply patches to nixpkgs source
      patchedSource = bootstrapPkgs.applyPatches {
        name = "nixpkgs-patched";
        src = nixpkgs;
        patches = map bootstrapPkgs.fetchpatch allNixpkgsPatches;
        prePatch = ''
          mkdir -p pkgs/by-name/an/antigravity
        '';
      };
      # Get eval-config from patched source
      evalConfig = import "${patchedSource}/nixos/lib/eval-config.nix";
      # Apply patches to home-manager source (only if there are patches)
      patchedHomeManagerSource =
        if allHomeManagerPatches == []
        then home-manager
        else
          bootstrapPkgs.applyPatches {
            name = "home-manager-patched";
            src = home-manager;
            patches = map bootstrapPkgs.fetchpatch allHomeManagerPatches;
          };
      # Import the patched home-manager flake
      patchedHomeManager =
        if allHomeManagerPatches == []
        then home-manager
        else import "${patchedHomeManagerSource}/flake.nix";
      # Get the NixOS module from the patched source
      patchedHomeManagerModule =
        if allHomeManagerPatches == []
        then home-manager.nixosModules.home-manager
        else import "${patchedHomeManagerSource}/nixos";
      # Create a modified inputs with patched home-manager
      patchedInputs = inputs // {
        home-manager = inputs.home-manager // {
          nixosModules = inputs.home-manager.nixosModules // {
            home-manager = patchedHomeManagerModule;
          };
          # Also provide the patched source path for any direct imports
          outPath = patchedHomeManagerSource.outPath or "${patchedHomeManagerSource}";
        };
      };
    in
      evalConfig {
        inherit system;
        modules = baseModules ++ modules;
        specialArgs =
          rec {
            inputs = patchedInputs;
            inherit machineNames;
            makeEnable = (import ./make-enable.nix) nixpkgs.lib;
            keys = import ./keys.nix;
            usersInfo = (import ./users.nix) {
              pkgs = {zsh = "zsh";};
              inherit keys system;
              inputs = patchedInputs;
            };
            realUsers = (
              builtins.attrNames
              (nixpkgs.lib.filterAttrs
                (_: value: (builtins.elem "isNormalUser" (builtins.attrNames value)) && value.isNormalUser)
                usersInfo.users.users)
            );
            mapAllKeysToValue = keys: value: builtins.listToAttrs (map (name: {inherit name value;}) keys);
            forEachUser = mapAllKeysToValue realUsers;
          }
          // specialArgs;
      };
  in {
    nixConfig = {
      substituters = [
        "https://cache.nixos.org/"
      ];
      trusted-public-keys = [
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      ];
      extra-substituters = [
        "http://192.168.1.26:5050"
        "https://cache.flox.dev"
      ];
      extra-trusted-public-keys = [
        "1896Folsom.duckdns.org:U2FTjvP95qwAJo0oGpvmUChJCgi5zQoG1YisoI08Qoo="
        "flox-cache-public-1:7F4OyH7ZCnFhcze3fJdfyXYLQw/aV7GEed86nQ7IsOs="
      ];
    };
    nixosConfigurations =
      builtins.mapAttrs (
        machineName: params: let
          machineParams =
            if builtins.hasAttr machineName customParams
            then (builtins.getAttr machineName customParams)
            else {};
        in
          mkConfig (params // machineParams)
      )
      defaultConfigurationParams;
  }
  //
  # Per-system packages (using flake-utils)
  inputs.flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { inherit system; };

      # Path to org-config.org in the dotfiles
      orgConfigOrg = ../dotfiles/emacs.d/org-config.org;

      # Tangle org-config.org and convert to setq format
      orgAgendaCustomConfig = pkgs.runCommand "org-agenda-custom-config" {
        buildInputs = [ pkgs.emacs-nox ];
      } ''
        mkdir -p $out
        mkdir -p work

        # Copy org file to writable location (tangle writes to same directory)
        cp ${orgConfigOrg} work/org-config.org

        # Tangle org-config.org
        emacs --batch \
          --eval '(require (quote org))' \
          --eval '(org-babel-tangle-file "work/org-config.org")'

        if [ -f "work/org-config-custom.el" ]; then
          # Use emacs to properly parse and convert s-expressions to setq
          emacs --batch \
            --eval "(with-temp-buffer
                      (insert-file-contents \"work/org-config-custom.el\")
                      (goto-char (point-min))
                      (let ((forms nil))
                        (condition-case nil
                            (while t
                              (let ((form (read (current-buffer))))
                                (when (and (listp form) (symbolp (car form)))
                                  (push (list 'setq (car form) (cadr form)) forms))))
                          (end-of-file nil))
                        (with-temp-file \"$out/custom-config.el\"
                          (dolist (form (nreverse forms))
                            (prin1 form (current-buffer))
                            (insert \"\n\")))))"
        else
          echo "Warning: org-config-custom.el not found after tangle"
          touch $out/custom-config.el
        fi

        # Append custom agenda commands and capture templates for the API
        cat >> $out/custom-config.el << 'ELISP'

;; Custom agenda commands for API
(setq org-agenda-custom-commands
      '(("n" "Next actions" todo "NEXT")
        ("s" "Started tasks" todo "STARTED")
        ("i" "Inbox" todo "INBOX")
        ("w" "Waiting tasks" todo "WAIT")
        ("h" "High priority" tags-todo "+PRIORITY<\"C\"")
        ("M" "Main view"
         ((agenda "" ((org-agenda-span 5)))
          (todo "NEXT")
          (todo "STARTED")
          (todo "INBOX")))))

;; Capture templates for API
(setq org-agenda-api-capture-templates
      '(("gtd-todo"
         :name "GTD Todo"
         :template ("t" "Todo" entry (file "/data/org/inbox.org")
                    "* INBOX %^{Title}\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
                    :immediate-finish t)
         :prompts (("Title" :type string :required t)))
        ("scheduled-todo"
         :name "Scheduled Todo"
         :template ("s" "Scheduled" entry (file "/data/org/inbox.org")
                    "* INBOX %^{Title}\nSCHEDULED: %^{When}t\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
                    :immediate-finish t)
         :prompts (("Title" :type string :required t)
                   ("When" :type date :required t)))
        ("deadline-todo"
         :name "Todo with Deadline"
         :template ("d" "Deadline" entry (file "/data/org/inbox.org")
                    "* INBOX %^{Title}\nDEADLINE: %^{When}t\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
                    :immediate-finish t)
         :prompts (("Title" :type string :required t)
                   ("When" :type date :required t)))))
ELISP
      '';

      # Build customized org-agenda-api container
      orgAgendaApiContainer = inputs.org-agenda-api.lib.${system}.mkContainer {
        customElispFile = "${orgAgendaCustomConfig}/custom-config.el";
      };

    in {
      packages = {
        org-agenda-custom-config = orgAgendaCustomConfig;
        org-agenda-api-container = orgAgendaApiContainer;
      };
    }
  );
}
