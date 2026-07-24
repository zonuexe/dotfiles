---
name: acp-agent-runner
description: >-
  Use this skill whenever the user wants a coding task carried out by an AI coding agent or model
  that is NOT Claude — for example OpenCode, GLM, Qwen, DeepSeek, Kimi, MiniMax, "another coding
  agent", "another vendor's model", or "whatever external agent is set up". Typical intents: hand
  or throw a task to that other agent and bring back its result; ask it to review changes, fix
  failing tests, or refactor; route work "through opencode"; or run the same job on two outside
  models to compare their outputs. This skill runs that external agent for you (over the Agent
  Client Protocol, in a sandbox). It applies in any language (English, 日本語, など) and to vague or
  indirect phrasing. Do NOT use it for Claude's own subagents (use the Agent tool instead), for
  plain Anthropic/LLM API scripts, or for running Claude itself — the defining signal is that the
  work goes to a different, non-Claude agent.
---

# Drive an external agent over ACP (safely)

Hand a self-contained task to an **external** agent (a different vendor's CLI/model) and get the
result back by speaking ACP — JSON-RPC over stdio — to it. This is **one-shot delegation, not a true
subagent**: there's no shared task list and no streaming into your own reasoning loop. Reach for it
when the value is *a different vendor's model* (a second opinion, a cross-model comparison, cost),
since Claude's own Agent tool can only run Claude.

## Safety first — the sandbox is non-negotiable

To let the external agent work unattended, the bundled client **auto-approves its file edits and
shell commands** (that's how it answers ACP permission requests). An external model running bash and
edits unattended is dangerous *outside* a sandbox — so always run it in a disposable, isolated copy:

- **Isolated working copy**, never the user's live repo — a throwaway `git worktree` or a `cp -Rc`
  clone. (A plain `git worktree` omits gitignored deps like `vendor/` / `node_modules/`; a `cp -Rc`
  clone keeps them, so the agent can actually build/test.)
- **Scoped cwd** = that copy; the agent can only read/write its cwd.
- **No installs / network / destructive ops** unless the user explicitly asked — say so in the prompt.
- **No commits** — leave edits in the working tree so you can diff and review.
- **Review before promoting** — nothing reaches the user's repo until you've read the diff.

The client auto-approves *because* it runs inside this sandbox. Never point it at an unsandboxed dir.

## Procedure

1. **Stage the sandbox** — make the disposable copy and put everything the agent needs *inside* it
   (it has only its cwd and your prompt — no memory of this chat, no access to Claude skills).
2. **Pick agent + model** — default agent: OpenCode. Model list and how selection works:
   [references/opencode.md](references/opencode.md).
3. **Run** — one command drives the whole ACP session:
   ```
   python <this-skill-dir>/scripts/acp_run.py --agent opencode --model <provider/model> \
     --cwd <sandbox> --prompt-file <task.txt> --out <out-dir> --timeout 1200
   ```
   It resolves the agent command (local `opencode` if installed, else `npx -y opencode-ai@latest`),
   sets the model, runs `initialize → session/new → session/prompt`, auto-approves permission
   requests, and writes `agent_message.txt`, `tool_calls.txt`, and `result.json` to `--out`.
4. **Review** — read `result.json` (`stopReason`, **`model_verified`**), the agent's final message,
   and `git diff` in the sandbox. Promote only what you trust.
5. **Teardown** — delete the sandbox copy.

## Writing the task prompt

Put everything in the prompt and as files in the sandbox: the task, the constraints (no
install/network/commit), and how to report results (e.g. "state the final state and what you
changed"). To make the agent follow a procedure (a skill, a checklist), stage those files in the
sandbox and tell it to read them by path — it won't have them otherwise.

## Examples

- **Second opinion:** "What would GLM-5.2 do about this failing test?" → stage a `cp -Rc` clone,
  write the task to `task.txt`, `acp_run.py --agent opencode --model opencode-go/glm-5.2 --cwd
  <clone> --prompt-file task.txt --out out/`, then review `out/result.json` + `git diff`.
- **Model comparison:** run the same task twice with different `--model` (e.g. `qwen3.7-max` vs
  `deepseek-v4-pro`) in separate clones, then diff the two results. Run them **sequentially, not in
  parallel** — OpenCode serialises through one SQLite db, so concurrent `acp` sessions fail with
  `database is locked` (see [references/opencode.md](references/opencode.md) § Gotchas).

## Troubleshooting

- **A model misreports its identity** (e.g. claims to be Claude). Ignore the prose; trust
  `result.json`'s `model_verified` (read back from the agent's own session record).
- **The agent didn't edit anything:** check the sandbox cwd is correct/writable and read
  `out/acp_stderr.log`; confirm auth (see references/opencode.md).
- **Hang or timeout:** raise `--timeout`; the last activity is in `out/acp_transcript.log`.

## Other agents

[references/agents.md](references/agents.md) is a catalog of ~39 ACP-compatible agents with their
launch commands (OpenCode, Gemini CLI `gemini --acp`, Qwen Code `qwen --acp`, Cursor `agent acp`,
Codex via `codex-acp`, Goose `goose acp`, Kimi `kimi acp`, and more), each with a confidence flag and
doc link. Treat it as a verified-as-of-research snapshot — re-check an entry before relying on it.

**Drive any of them right now** with `--cmd` (no code change): pick the launch command from the
catalog and pass it through —
`python <this-skill-dir>/scripts/acp_run.py --cmd "qwen --acp" --cwd <sandbox> --prompt-file
<task.txt> --out <out>`. Model selection and verification are skipped on this path (they're
agent-specific), so set the model however that agent expects and confirm it yourself.

**Grok (xAI)** is the worked second example after OpenCode — verified end-to-end here on the
`--cmd` path. Its ACP server is a subcommand and its model goes on the command line:

```
python <this-skill-dir>/scripts/acp_run.py \
  --cmd "grok agent -m grok-composer-2.5-fast --always-approve stdio" \
  --cwd <sandbox> --prompt-file <task.txt> --out <out>
```

Full details — model ids, auth (`grok login` / `XAI_API_KEY`), the `-m`-before-`stdio` gotcha,
and why `model_verified` is null for Grok (no programmatic session-model readback) — are in
[references/grok.md](references/grok.md).

**For first-class support** (automatic model setup + verification like OpenCode has), add an entry to
the `AGENTS` registry in `scripts/acp_run.py` — each supplies a command resolver, model setup, and
model verification; the ACP protocol code is agent-agnostic, so only those three hooks differ. To add
one safely:

1. **Confirm it speaks ACP** against the authoritative directory:
   <https://agentclientprotocol.com/get-started/agents> (the canonical list of ACP-compatible
   agents). Don't invent support — if it's not there, it likely needs a different transport.
2. **Get its ACP launch command from its own docs** — that directory lists agents but *not* launch
   commands (each links to its own documentation). There's no single registry of commands, so this
   step is per-agent.
3. **Verify the command yourself** (handshake + a trivial prompt) before trusting it; CLIs change.
   Record the verified command in the registry with a comment citing where it came from — treat the
   registry as a *verified snapshot*, not invented defaults.

Do not fetch these at runtime: the directory carries no commands, and the skill should run
deterministically offline. Provenance lives in code comments and `references/`, refreshed by hand.
