# ACP agent catalog

Launch commands for ACP-compatible agents (stdio JSON-RPC). Snapshot against
<https://agentclientprotocol.com/get-started/agents> — re-check before use.

- **Full table + notes:** [agents-catalog.md](agents-catalog.md), [agents-notes.md](agents-notes.md)
- **OpenCode (first-class):** [opencode.md](opencode.md) — `acp_run.py --agent opencode`
- **Grok (`--cmd` example):** [grok.md](grok.md)

Common launches:

| Agent | Command |
|-------|---------|
| OpenCode | `opencode acp` |
| Grok | `grok agent -m <id> --always-approve stdio` |
| Qwen Code | `qwen --acp` |
| Gemini CLI | `gemini --acp` |
| Cursor | `agent acp` |
| Goose | `goose acp` |
| Kimi | `kimi acp` |
| codex-acp | `codex-acp` |

## One-off via `--cmd`

```
python <skill-dir>/scripts/acp_run.py --cmd "qwen --acp" \
  --cwd <workspace> --prompt-file <task.txt> --out <out>
```

Model setup and `model_verified` are skipped — set the model how that agent expects.

Grok example:

```
python <skill-dir>/scripts/acp_run.py \
  --cmd "grok agent -m grok-composer-2.5-fast --always-approve stdio" \
  --cwd <workspace> --prompt-file <task.txt> --out <out>
```

## First-class registry entry

Add to `AGENTS` in `scripts/acp_run.py` (command + set_model + verify_model).

1. Confirm the agent is in the ACP directory (link above).
2. Get the launch command from **that agent's** docs.
3. Verify with a live handshake; record provenance in a code comment.

Keep the skill offline — do not fetch agent lists at runtime.
