# ACP agent catalog

How each ACP-compatible agent is launched as an ACP server (stdio JSON-RPC). Researched from
each agent's own docs against the directory at <https://agentclientprotocol.com/get-started/agents>.
`acp_run.py` ships a verified first-class registry entry (auto model setup + verification) for
**OpenCode** only; to drive any other agent, pass its launch command via `--cmd` (see SKILL.md).
**Grok (xAI)** is verified end-to-end on the `--cmd` path and has its own worked reference —
[grok.md](grok.md) — with the exact launch command, model ids, auth, and its
no-programmatic-model-verification caveat. **Verify before trusting — CLIs change.**

Confidence: **verified** = command explicit in official docs; **likely** = inferred/secondary source; **unknown** = not found.

| Agent | ACP launch | No-install alt | Auth | Conf. | Doc |
|---|---|---|---|---|---|
| AgentPool | `agentpool serve-acp <agents.yml>` | `uvx --python 3.13 agentpool@latest serve-acp` | provider env vars (e.g. OPENAI_API_KEY… | verified | [link](https://phil65.github.io/agentpool/advanced/acp-integration/) |
| Augment Code | `auggie --acp` | `npx -y @augmentcode/auggie --acp` | Augment account login | verified | [link](https://docs.augmentcode.com/cli/acp) |
| AutoDev | `xiuper acp-agent` | `npx @xiuper/cli acp-agent` | model/provider API key | verified | [link](https://github.com/phodal/auto-dev/issues/535) |
| Blackbox AI | `blackbox --experimental-acp` | `npm install -g @blackbox/cli` | BLACKBOX_API_KEY env | verified | [link](https://docs.blackbox.ai/features/blackbox-cli/acp-integration) |
| Bub | — (needs adapter/bridge — see notes) | `uvx bub` | BUB_API_KEY / bub login | verified | [link](https://github.com/bubbuild/bub) |
| bub-acp-server | `bub acp serve` (or standalone `bub-acp-server`) | `uv pip install git+https://github.com/bubbuild/bub-contrib.git#subdirectory=packages/bub-acp-server` | reuses Bub config/creds | verified | [link](https://github.com/bubbuild/bub-contrib/tree/main/packages/bub-acp-server) |
| Claude Agent (Anthropic Agent SDK) | — (needs adapter/bridge — see notes) | — | ANTHROPIC_API_KEY (or Bedrock/Vertex/A… | verified | [link](https://code.claude.com/docs/en/agent-sdk/overview) |
| claude-agent-acp (Zed adapter) | `claude-agent-acp` | `npx @agentclientprotocol/claude-agent-acp` | ANTHROPIC_API_KEY (via Claude Agent SDK) | verified | [link](https://github.com/zed-industries/claude-agent-acp) |
| Cline | `cline --acp` | — | cline auth | verified | [link](https://docs.cline.bot/cli/acp-editor-integrations) |
| Code Assistant | `code-assistant acp` | — | ANTHROPIC_API_KEY/OPENAI_API_KEY or pr… | verified | [link](https://github.com/stippi/code-assistant?tab=readme-ov-file#configuration) |
| Codex CLI (OpenAI) | — (needs adapter/bridge — see notes) | — | codex login or OPENAI_API_KEY/CODEX_AP… | verified | [link](https://developers.openai.com/codex/cli) |
| codex-acp (Zed adapter) | `codex-acp` | `npx @zed-industries/codex-acp` | OPENAI_API_KEY (also CODEX_API_KEY / C… | verified | [link](https://github.com/zed-industries/codex-acp) |
| Cursor | `agent acp` | — | agent login, or CURSOR_API_KEY/CURSOR_… | verified | [link](https://cursor.com/docs/cli/acp) |
| Docker cagent | `cagent acp <agent.yaml>` | `docker agent` | provider API keys via env referenced i… | verified | [link](https://zed.dev/acp/agent/docker-cagent) |
| Factory Droid | `droid exec --output-format acp` | `npx droid exec --output-format acp` | Factory AI account login | verified | [link](https://zed.dev/acp/agent/factory-droid) |
| fast-agent | `fast-agent-acp` | `uv run <agent.py> --transport acp` | provider API key env (OPENAI_API_KEY/A… | verified | [link](https://fast-agent.ai/acp) |
| Gemini CLI | `gemini --acp` | `npx @google/gemini-cli` | Google login (free tier) / GOOGLE_API_… | verified | [link](https://github.com/google-gemini/gemini-cli/blob/main/docs/cli/acp-mode.md) |
| GitHub Copilot CLI | `copilot --acp` | `npx @github/copilot` | copilot then /login, or PAT w/ Copilot… | verified | [link](https://docs.github.com/en/copilot/reference/copilot-cli-reference/acp-server) |
| Goose | `goose acp` | — | GOOSE_PROVIDER/GOOSE_MODEL; ACP auth v… | verified | [link](https://block.github.io/goose/docs/guides/acp-clients) |
| Grok CLI (xAI) | `grok agent stdio` (model via `grok agent -m <id> … stdio`) | — | `grok login` or `XAI_API_KEY` | verified | [link](https://docs.x.ai/build/cli/headless-scripting#acp) |
| Hermes Agent | `hermes acp` | `python -m acp_adapter` | hermes model / ~/.hermes/.env | verified | [link](https://hermes-agent.nousresearch.com/docs/user-guide/features/acp) |
| Kimi CLI | `kimi acp` | `uvx kimi-cli` | kimi then /login | verified | [link](https://moonshotai.github.io/kimi-cli/en/reference/kimi-command.html) |
| Kiro CLI | `kiro-cli acp` | — | kiro-cli login (Builder ID / Google / … | verified | [link](https://kiro.dev/docs/cli/acp/) |
| Minion Code | `mcode acp` | — | LLM provider via Minion config files | verified | [link](https://github.com/femto/minion-code) |
| Mistral Vibe | `vibe-acp` | `uvx mistral-vibe` | MISTRAL_API_KEY env/.env; set up via m… | verified | [link](https://github.com/mistralai/mistral-vibe/blob/main/docs/acp-setup.md) |
| OpenClaw | `openclaw acp` | — | Remote Gateway: openclaw acp --url wss… | verified | [link](https://docs.openclaw.ai/cli/acp) |
| OpenCode | `opencode acp` | `npx -y opencode-ai@latest acp` | opencode auth login | verified | [link](https://opencode.ai/docs/acp/) |
| OpenHands | `openhands acp` | — | ~/.openhands/settings.json; configure … | verified | [link](https://docs.openhands.dev/openhands/usage/run-openhands/acp) |
| Pi (coding-agent) | — (needs adapter/bridge — see notes) | — | ANTHROPIC_API_KEY or pi /login | verified | [link](https://github.com/badlogic/pi-mono/tree/main/packages/coding-agent) |
| pi-acp adapter | `pi-acp` | `npx -y pi-acp` | inherits pi auth; pi-acp --terminal-lo… | verified | [link](https://github.com/svkozak/pi-acp) |
| Poolside (pool) | `pool acp` | — | POOLSIDE_API_KEY or ~/.config/poolside… | verified | [link](https://github.com/poolsideai/pool) |
| Qoder CLI | `qoder acp` | `npm i @qoder-ai/qodercli` | /login or QODER_PERSONAL_ACCESS_TOKEN | verified | [link](https://docs.qoder.com/cli/acp) |
| Qwen Code | `qwen --acp` | `npx @qwen-code/qwen-code` | qwen then /auth (Qwen OAuth / OpenAI /… | verified | [link](https://github.com/QwenLM/qwen-code) |
| siGit Code | `sigit` (auto-detects ACP; no flag) | `uvx --from sigit-code sigit` | none (local) | verified | [link](https://github.com/getsigit/sigit) |
| Stakpak | `stakpak acp` | — | stakpak login --api-key $STAKPAK_API_K… | verified | [link](https://github.com/stakpak/agent?tab=readme-ov-file#agent-client-protocol-acp) |
| VT Code | `vtcode acp` | `cargo install vtcode` | provider API key via vtcode.toml/env | verified | [link](https://github.com/vinhnx/vtcode/blob/main/docs/guides/zed-acp.md) |
| crow-cli | `crow-cli acp` | `uvx crow-cli` | ~/.crow/config.yaml (OpenAI-compatible) | likely | [link](https://github.com/crow-cli/crow-cli) |
| fount | `deno run ... fount_ide_agent.mjs?fount-apikey=KEY&charname=ROLE` | `deno run --allow-env --allow-net <fount_ide_agent.mjs URL>` | fount API key (?fount-apikey= or FOUNT… | likely | [link](https://github.com/steve02081504/fount) |
| Junie (JetBrains) | `junie --acp true` | `npx @jetbrains/junie-cli` | JetBrains login / JUNIE_API_KEY / BYOK | likely | [link](https://junie.jetbrains.com/docs/parameters.html) |
| stdio Bus | — (needs adapter/bridge — see notes) | — | n/a | unknown | [link](https://github.com/stdiobus/stdiobus) |

## Notes & special cases
- **AgentPool** — ACP server takes a config file arg; JSON-RPC over stdio. Flags --show-messages, --log-level.
- **Augment Code** — JSON-RPC over stdio; --acp flag.
- **AutoDev** — Bidirectional ACP (agent+client). Pkg name from README; command from issue #535.
- **Blackbox AI** — ACP experimental (--experimental-acp).
- **Bub** — Core Bub has NO acp subcommand; ACP via separate bub-acp-server plugin (-> 'bub acp serve').
- **bub-acp-server** — Plugin adding ACP to Bub. Also standalone 'bub-acp-server'. Zed args ['acp','serve'].
- **Claude Agent (Anthropic Agent SDK)** — Library, NOT an ACP server. For ACP use the claude-agent-acp adapter (next).
- **claude-agent-acp (Zed adapter)** — Running the binary IS the ACP agent over stdio; no subcommand. Wraps Claude Agent SDK.
- **Cline** — --acp flag (CLI >2.0.0) -> ACP over stdio. Zed args ['--acp'].
- **Code Assistant** — 'code-assistant acp' over stdio; optional --model.
- **Codex CLI (OpenAI)** — NO native 'codex acp'. Use codex-acp adapter (next). 'codex mcp-server' is MCP not ACP.
- **codex-acp (Zed adapter)** — Binary IS the ACP agent over stdio; no subcommand.
- **Cursor** — Binary is 'agent'. ACP over stdio (ndjson JSON-RPC).
- **Docker cagent** — Takes an agent YAML arg. Docker Desktop form: 'docker agent serve acp <file|ref>'.
- **Factory Droid** — ACP = 'exec' subcommand with --output-format acp.
- **fast-agent** — Dedicated 'fast-agent-acp' binary. Also 'fast-agent serve --transport acp'. Flags -x, --no-permissions, --model.
- **Gemini CLI** — `gemini --acp` over stdio. Old '--experimental-acp' deprecated.
- **GitHub Copilot CLI** — stdio default; explicit 'copilot --acp --stdio'. Public preview.
- **Goose** — `goose acp` over stdio. Auto-loads MCP from client.
- **Grok CLI (xAI)** — ACP is the `agent stdio` subcommand (NOT `grok --acp`); `grok agent headless`/`serve` are WebSocket, not ACP. Model on the `agent` parent: `grok agent -m <id> --always-approve stdio` (`-m` before `stdio`). Auto-approve = `--always-approve`. Auth grok.com / `XAI_API_KEY`. No programmatic model verification (`grok export` is Markdown-only) → `model_verified` is null on the --cmd path; trust the `-m` flag + `grok models`. Separate session db from OpenCode (can overlap OpenCode runs). VERIFIED end-to-end in this project. Full reference: [grok.md](grok.md).
- **Hermes Agent** — Also 'hermes-acp'. Logs to stderr. Optional --setup-browser.
- **Kimi CLI** — `kimi acp` (JSON-RPC stdio). Deprecated '--acp' also exists.
- **Kiro CLI** — JSON-RPC 2.0 over stdio. Optional --agent <name>. Use full path to kiro-cli in editor config.
- **Minion Code** — ACP for Zed etc. Flags: --dir, --model, --verbose, --dangerously-skip-permissions.
- **Mistral Vibe** — Separate binary 'vibe-acp' (NOT 'vibe acp'); JSON-RPC over stdio; empty args in Zed.
- **OpenClaw** — ACP over stdio, forwards to Gateway over WebSocket.
- **OpenCode** — ACP subprocess over stdio; Zed args ["acp"]. VERIFIED end-to-end in this project.
- **OpenHands** — ACP server; flags --llm-approve, --always-approve, --resume <id>.
- **Pi (coding-agent)** — Pi itself is NOT ACP (has own 'pi --mode rpc'). Use pi-acp adapter.
- **pi-acp adapter** — Bare 'pi-acp' IS the ACP server (no subcommand); spawns 'pi --mode rpc'.
- **Poolside (pool)** — 'pool acp'; flags forwardable e.g. --reasoning high.
- **Qoder CLI** — 'qoder acp' (older qodercli/--acp is stale).
- **Qwen Code** — 'qwen --acp' stable (PR #1355); '--experimental-acp' deprecated. 'qwen serve' = ACP over HTTP/SSE (not stdio).
- **siGit Code** — No subcommand/flag — point client command at the 'sigit' binary; auto-detects ACP over stdio.
- **Stakpak** — `stakpak acp` over stdio. Zed args ['acp'].
- **VT Code** — `vtcode acp` over stdio. Zed env VT_ACP_ENABLED=1, VT_ACP_ZED_ENABLED=1.
- **crow-cli** — Native ACP agent; acp subcommand per Zed registry, not explicit in README.
- **fount** — No 'acp' subcommand; stdio<->WebSocket bridge via Deno script.
- **Junie (JetBrains)** — Parameters page lists --acp; README omits it -> likely.
- **stdio Bus** — NOT a coding agent — a JSON-RPC/NDJSON router & process supervisor. No agent ACP launch. EXCLUDE.
