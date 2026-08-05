- Unless the user or task explicitly says otherwise, start every agent session
  or worker created by Paseo in the provider's full-permissions mode (Codex:
  `full-access`; use the equivalent full-permissions/bypass mode for other
  providers). This runtime setting does not by itself authorize remote pushes,
  destructive actions, or other side effects beyond the user's request.
- When both mechanisms are available, prefer a Paseo-managed subagent for
  substantive delegation.
- Use a provider-native child only for small, ephemeral work that belongs inside
  the current provider turn: the same provider is appropriate, no separate
  workspace is needed, no follow-up or intervention is expected, and the parent
  will consume the result immediately.
- A provider-native child's timeline may be visible in Paseo, but visibility is
  not equivalent to management; provider-owned panes can be read-only and their
  lifecycle remains with the provider.
- Give every Paseo-managed subagent a self-contained initial prompt. It does not
  inherit the parent's full conversation automatically.
