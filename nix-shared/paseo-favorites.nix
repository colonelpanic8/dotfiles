# Paseo model favorites: the daemon's agent profiles and the command center
# model shortcuts are both derived from this list so they cannot drift apart.
let
  favorites = [
    {
      name = "Fable 5.1";
      provider = "claude";
      model = "claude-fable-5-1";
      shortcut = "F13";
    }
    {
      name = "GPT-6-Sol";
      provider = "codex";
      model = "gpt-6-sol";
      shortcut = "F14";
    }
    {
      name = "GPT-6-Luna";
      provider = "codex";
      model = "gpt-6-luna";
      shortcut = "F15";
    }
    {
      name = "Opus 5.5";
      provider = "claude";
      model = "claude-opus-5-5";
      shortcut = "F16";
    }
  ];
in {
  agentProfiles =
    map (f: {
      id = "${f.provider}:${f.model}";
      inherit (f) name provider model;
    })
    favorites;

  keyboardShortcutOverrides = builtins.listToAttrs (map (f: {
      name = "command-center.shortcut:models:${f.provider}:${f.model}";
      value = f.shortcut;
    })
    favorites);
}
