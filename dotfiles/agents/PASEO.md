# Paseo-hosted Agent Preferences

These instructions apply only to agent sessions launched and managed by Paseo. The global `AGENTS.md` loads this file when `PASEO_AGENT_ID` is set.

## Paseo-managed vs provider-native subagents

- When both mechanisms are available, prefer a Paseo-managed subagent for substantive delegation. Paseo-managed agents are independently inspectable and promptable, can use a different provider or workspace, and can be detached or resumed.
- Use a provider-native child only for small, ephemeral work that belongs inside the current provider turn: the same provider is appropriate, no separate workspace is needed, no follow-up or intervention is expected, and the parent will consume the result immediately.
- A provider-native child's timeline may be visible in Paseo, but visibility is not equivalent to management; provider-owned panes can be read-only and their lifecycle remains with the provider.
- Give every Paseo-managed subagent a self-contained initial prompt. It does not inherit the parent's full conversation automatically.
