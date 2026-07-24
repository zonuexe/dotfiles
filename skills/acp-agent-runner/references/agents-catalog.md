# ACP agent catalog

How each ACP-compatible agent is launched as an ACP server (stdio JSON-RPC). Researched from
each agent's own docs against the directory at <https://agentclientprotocol.com/get-started/agents>.
`acp_run.py` ships a verified first-class registry entry (auto model setup + verification) for
**OpenCode** only; to drive any other agent, pass its launch command via `--cmd` (see SKILL.md).
**Grok (xAI)** is verified end-to-end on the `--cmd` path and has its own worked reference —
[grok.md](grok.md) — with the exact launch command, model ids, auth, and its
no-programmatic-model-verification caveat. **Verify before trusting — CLIs change.**

Confidence: **verified** = command explicit in official docs; **likely** = inferred/secondary source; **unknown** = not found.

| Agent | ACP launch | Conf. | Doc |
|---|---|---|---|
| AgentPool | `agentpool serve-acp <agents.yml>` | `uvx --python 3.13 agentpool@latest serve-acp` | provider env vars (e.g. OPENAI_API_KEY… | verified | [link](https://phil65.github.io/agentpool/advanced/acp-integration/) |
| Augment Code | `auggie --acp` | verified | [link](https://docs.augmentcode.com/cli/acp) |
| AutoDev | `xiuper acp-agent` | verified | [link](https://github.com/phodal/auto-dev/issues/535) |
| Blackbox AI | `blackbox --experimental-acp` | verified | [link](https://docs.blackbox.ai/features/blackbox-cli/acp-integration) |
| Bub | — (needs adapter/bridge — see notes) | verified | [link](https://github.com/bubbuild/bub) |
| bub-acp-server | `bub acp serve` (or standalone `bub-acp-server`) | verified | [link](https://github.com/bubbuild/bub-contrib/tree/main/packages/bub-acp-server) |
| Claude Agent (Anthropic Agent SDK) | — (needs adapter/bridge — see notes) | — | ANTHROPIC_API_KEY (or Bedrock/Vertex/A… | verified | [link](https://code.claude.com/docs/en/agent-sdk/overview) |
| claude-agent-acp (Zed adapter) | `claude-agent-acp` | `npx @agentclientprotocol/claude-agent-acp` | ANTHROPIC_API_KEY (via Claude Agent SDK) | verified | [link](https://github.com/zed-industries/claude-agent-acp) |
| Cline | `cline --acp` | verified | [link](https://agentclientprotocol.com/get-started/agents) |
| Code Assistant | `code-assistant acp` | verified | [link](https://github.com/stippi/code-assistant?tab=readme-ov-file#configuration) |
| Codex CLI (OpenAI) | — (needs adapter/bridge — see notes) | verified | [link](https://developers.openai.com/codex/cli) |
| codex-acp (Zed adapter) | `codex-acp` | verified | [link](https://github.com/zed-industries/codex-acp) |
| Cursor | `agent acp` | verified | [link](https://cursor.com/docs/cli/acp) |
| Docker cagent | `cagent acp <agent.yaml>` | verified | [link](https://zed.dev/acp/agent/docker-cagent) |
| Factory Droid | `droid exec --output-format acp` | verified | [link](https://zed.dev/acp/agent/factory-droid) |
| fast-agent | `fast-agent-acp` | verified | [link](https://fast-agent.ai/acp) |
| Gemini CLI | `gemini --acp` | verified | [link](https://github.com/google-gemini/gemini-cli/blob/main/docs/cli/acp-mode.md) |
| GitHub Copilot CLI | `copilot --acp` | verified | [link](https://docs.github.com/en/copilot/reference/copilot-cli-reference/acp-server) |
| Goose | `goose acp` | verified | [link](https://agentclientprotocol.com/get-started/agents) |
| Grok CLI (xAI) | `grok agent stdio` (model via `grok agent -m <id> … stdio`) | verified | [link](https://docs.x.ai/build/cli/headless-scripting#acp) |
| Hermes Agent | `hermes acp` | `python -m acp_adapter` | hermes model / ~/.hermes/.env | verified | [link](https://hermes-agent.nousresearch.com/docs/user-guide/features/acp) |
| Kimi CLI | `kimi acp` | verified | [link](https://moonshotai.github.io/kimi-cli/en/reference/kimi-command.html) |
| Kiro CLI | `kiro-cli acp` | verified | [link](https://kiro.dev/docs/cli/acp/) |
| Minion Code | `mcode acp` | verified | [link](https://github.com/femto/minion-code) |
| Mistral Vibe | `vibe-acp` | verified | [link](https://github.com/mistralai/mistral-vibe/blob/main/docs/acp-setup.md) |
| OpenClaw | `openclaw acp` | verified | [link](https://docs.openclaw.ai/cli/acp) |
| OpenCode | `opencode acp` | verified | [link](https://opencode.ai/docs/acp/) |
| OpenHands | `openhands acp` | verified | [link](https://docs.openhands.dev/openhands/usage/run-openhands/acp) |
| Pi (coding-agent) | — (needs adapter/bridge — see notes) | verified | [link](https://github.com/badlogic/pi-mono/tree/main/packages/coding-agent) |
| pi-acp adapter | `pi-acp` | verified | [link](https://github.com/svkozak/pi-acp) |
| Poolside (pool) | `pool acp` | verified | [link](https://github.com/poolsideai/pool) |
| Qoder CLI | `qoder acp` | verified | [link](https://docs.qoder.com/cli/acp) |
| Qwen Code | `qwen --acp` | verified | [link](https://github.com/QwenLM/qwen-code) |
| siGit Code | `sigit` (auto-detects ACP; no flag) | verified | [link](https://github.com/getsigit/sigit) |
| Stakpak | `stakpak acp` | verified | [link](https://github.com/stakpak/agent?tab=readme-ov-file#agent-client-protocol-acp) |
| VT Code | `vtcode acp` | verified | [link](https://github.com/vinhnx/vtcode/blob/main/docs/guides/zed-acp.md) |
| crow-cli | `crow-cli acp` | likely | [link](https://github.com/crow-cli/crow-cli) |
| fount | `deno run ... fount_ide_agent.mjs?fount-apikey=KEY&charname=ROLE` | likely | [link](https://github.com/steve02081504/fount) |
| Junie (JetBrains) | `junie --acp true` | likely | [link](https://junie.jetbrains.com/docs/parameters.html) |
| stdio Bus | — (needs adapter/bridge — see notes) | unknown | [link](https://github.com/stdiobus/stdiobus) |

Notes: [agents-notes.md](agents-notes.md).
