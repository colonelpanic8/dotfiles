# Hyprscratch Migration Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Replace custom toggle-scratchpad.sh + windowrulev2 scratchpad setup with hyprscratch, gaining auto-dismiss on workspace change via `clean` mode.

**Architecture:** Add hyprscratch as a flake input, wire its home-manager module into hyprland.nix with all 9 scratchpad definitions, simplify hyprland.conf keybinds, and remove the now-redundant window rules and toggle script.

**Tech Stack:** NixOS flake, home-manager module, hyprscratch 0.6.4, Hyprland IPC

---

### Task 1: Add hyprscratch flake input

**Files:**
- Modify: `flake.nix` (add input after `hyprland-plugins` block, ~line 71)

**Step 1: Add the flake input**

In `flake.nix`, after the `hyprland-plugins` input block (line 71), add:

```nix
    hyprscratch = {
      url = "github:sashetophizika/hyprscratch";
      inputs.nixpkgs.follows = "nixpkgs";
    };
```

**Step 2: Verify flake input resolves**

Run: `cd /home/imalison/dotfiles/nixos && nix flake lock --update-input hyprscratch`
Expected: Lock file updated, no errors.

---

### Task 2: Wire hyprscratch home-manager module into hyprland.nix

**Files:**
- Modify: `hyprland.nix`

**Step 1: Add hyprscratch home-manager module with scratchpad config**

Replace the existing `home-manager.sharedModules` block (lines 11-18) with one that includes both kanshi and the hyprscratch module:

```nix
  home-manager.sharedModules = [
    inputs.hyprscratch.homeModules.default
    {
      services.kanshi = {
        enable = true;
        systemdTarget = "hyprland-session.target";
      };

      programs.hyprscratch = {
        enable = true;
        settings = {
          daemon_options = "clean";
          global_options = "persist";
          global_rules = "float;size 90% 90%;center";

          htop = {
            command = "alacritty --class htop-scratch --title htop -e htop";
            class = "htop-scratch";
          };

          volume = {
            command = "pavucontrol";
            class = "pavucontrol";
          };

          spotify = {
            command = "spotify";
            class = "spotify";
          };

          element = {
            command = "element-desktop";
            class = "Element";
          };

          slack = {
            command = "slack";
            class = "Slack";
          };

          transmission = {
            command = "transmission-gtk";
            class = "transmission-gtk";
          };

          dropdown = {
            command = "ghostty --config-file=/home/imalison/.config/ghostty/dropdown";
            class = "com.mitchellh.ghostty.dropdown";
            rules = "float;size 100% 50%;move 0 0;noborder;noshadow;animation slide";
          };

          gmail = {
            command = "google-chrome-stable --new-window https://mail.google.com/mail/u/0/#inbox";
            class = "google-chrome";
            title = "Gmail";
          };

          messages = {
            command = "google-chrome-stable --new-window https://messages.google.com/web/conversations";
            class = "google-chrome";
            title = "Messages";
          };
        };
      };
    }
  ];
```

Note: `wlsunset` was removed from packages in git status (already handled elsewhere). No change needed there.

---

### Task 3: Update hyprland.conf — remove scratchpad window rules

**Files:**
- Modify: `~/dotfiles/dotfiles/config/hypr/hyprland.conf`

**Step 1: Remove all scratchpad windowrulev2 rules**

Delete lines 210-258 (all `windowrulev2` rules for scratchpad classes: htop-scratch, pavucontrol, spotify, Element, Slack, transmission-gtk, dropdown ghostty, gmail chrome, messages chrome). These are the `workspace special:* silent`, `float`, `size`, `center`, `move`, `noborder`, `noshadow`, `animation` rules for scratchpad windows.

Keep all other windowrulev2 rules (float dialogs, PiP, Open File, Save File, Confirm — lines 203-208).

---

### Task 4: Update hyprland.conf — replace scratchpad keybinds

**Files:**
- Modify: `~/dotfiles/dotfiles/config/hypr/hyprland.conf`

**Step 1: Replace toggle keybinds**

Replace the scratchpad toggle section (lines 293-302) with simplified hyprscratch calls:

```
# Toggle scratchpads
bind = $modAlt, E, exec, hyprscratch toggle element
bind = $modAlt, G, exec, hyprscratch toggle gmail
bind = $modAlt, H, exec, hyprscratch toggle htop
bind = $modAlt, M, exec, hyprscratch toggle messages
bind = $modAlt, K, exec, hyprscratch toggle slack
bind = $modAlt, S, exec, hyprscratch toggle spotify
bind = $modAlt, T, exec, hyprscratch toggle transmission
bind = $modAlt, V, exec, hyprscratch toggle volume
bind = $modAlt, grave, exec, hyprscratch toggle dropdown
```

**Step 2: Remove "move to scratchpad" keybinds**

Delete lines 305-312 (the `movetoworkspace, special:*` bindings). These relied on special workspaces; hyprscratch manages window placement internally.

**Step 3: Remove commented-out exec-once scratchpad spawns**

Delete lines 590-598 (the commented-out `exec-once` lines for scratchpad apps). Hyprscratch spawns on first toggle.

---

### Task 5: Build and test

**Step 1: Build the NixOS configuration**

Run: `cd /home/imalison/dotfiles/nixos && just switch`
Expected: Successful build and activation.

**Step 2: Verify hyprscratch service is running**

Run: `systemctl --user status hyprscratch`
Expected: Active (running).

**Step 3: Test a scratchpad toggle**

Run: `hyprscratch toggle htop`
Expected: htop scratchpad appears. Running again hides it.

**Step 4: Test auto-dismiss on workspace change**

1. Toggle a scratchpad (e.g. `hyprscratch toggle spotify`)
2. Switch workspace with Mod+2
3. Expected: Scratchpad auto-hides.

**Step 5: Commit**

```bash
git add flake.nix flake.lock hyprland.nix
git add -u ../dotfiles/config/hypr/hyprland.conf
git commit -m "feat: migrate scratchpads to hyprscratch with auto-dismiss on workspace change"
```

---

## Notes

- **workspace-history.sh stays** — it tracks workspace back/forth history, unrelated to scratchpad management.
- **toggle-scratchpad.sh can be deleted** after confirming everything works. Not deleted in this plan to keep a rollback path.
- **NSP hidden workspace** (Mod+X / Mod+Shift+X) is unaffected — it uses `togglespecialworkspace` directly, not the scratchpad system.
- **The `special` option** is NOT used — hyprscratch manages windows on normal workspaces by default. The `clean` daemon option handles auto-hiding. If the overlay behavior is preferred, add `special` to `global_options`.
