---
name: acp-agent-runner
description: >-
  Use this skill whenever the user wants a coding task carried out by an AI coding agent or model
  that is NOT Claude — for example OpenCode, GLM, Qwen, DeepSeek, Kimi, MiniMax, "another coding
  agent", "another vendor's model", or "whatever external agent is set up". Typical intents: hand
  or throw a task to that other agent and bring back its result; ask it to review changes, fix
  failing tests, or refactor; route work "through opencode"; or run the same job on two outside
  models to compare their outputs. This skill runs that external agent for you (over the Agent
  Client Protocol), in an isolated worktree by default or in-place when the user chooses.
  It applies in any language (English, 日本語, など) and to vague or indirect phrasing. Do NOT use
  it for Claude's own subagents (use the Agent tool instead), for plain Anthropic/LLM API scripts,
  or for running Claude itself — the defining signal is that the work goes to a different,
  non-Claude agent.
---

# Drive an external agent over ACP (safely)

Hand a self-contained task to an **external** agent (a different vendor's CLI/model) and get the
result back by speaking ACP — JSON-RPC over stdio — to it. This is **one-shot delegation, not a true
subagent**: there's no shared task list and no streaming into your own reasoning loop. Reach for it
when the value is *a different vendor's model* (a second opinion, a cross-model comparison, cost),
since Claude's own Agent tool can only run Claude.

## Workspace mode — pick before you run

The bundled client **auto-approves** the external agent's file edits and shell commands (ACP
permission requests). That is powerful and unsafe if pointed at the wrong tree. Always choose a
**workspace mode** first:

| Mode | `--cwd` points at | When to use |
|------|-------------------|-------------|
| **`isolated`** (default) | Disposable copy: `git worktree` or `cp -Rc` clone | Second opinions, experiments, multi-model comparison, untrusted/large refactors, anything you may discard |
| **`inplace`** | The live working tree (repo root or a subdir the user named) | User explicitly wants edits on the current branch; small fixes; they accept risk |

### How to choose

1. If the user says **isolate / worktree / sandbox / throwaway / compare models** → **`isolated`**.
2. If the user says **in place / this repo / current branch / don't copy / edit here** → **`inplace`**.
3. If they don't say → **default to `isolated`**, and say so briefly when you start ("running isolated
   in a worktree; say if you want in-place instead").

Do **not** silently use `inplace` for multi-model fan-out or open-ended "fix everything" tasks.

### Mode: `isolated` (default)

- Stage a disposable copy, never the live tree as the only working copy:
  - `git worktree add <path> -b acp/<task-slug>` (or detached HEAD) — light, but **omits**
    gitignored deps (`vendor/`, `node_modules/`).
  - `cp -Rc <repo> <sandbox>` — heavier; **keeps** ignored deps so build/test works.
- `--cwd` = that copy. Review `git diff` there; **promote only what you trust**.
- Teardown: remove the worktree (`git worktree remove`) or delete the clone when done.
- Still: no installs/network/destructive ops unless asked; **no commits** unless the user asked
  for commits.

### Mode: `inplace`

- `--cwd` = the path the user wants edited (usually the repo root of the current session).
- **Confirm once** if the request was ambiguous: auto-approve will write and run shell in that tree.
- Still put **no installs / no network / no destructive ops / no commits** in the task prompt unless
  the user explicitly allowed them.
- Review with `git diff` / `git status` in that same tree — there is no separate promote step; the
  edits already land where the user is working. Prefer leaving uncommitted so they can revert.
- OpenCode may drop `opencode.json` / `.opencode/` into `--cwd` — strip or leave per user preference
  when reviewing the diff.

Common rules for **both** modes:

- The agent sees only its **cwd + your prompt** (no chat memory, no Claude skills unless you stage
  files into cwd and tell it to read them).
- Never skip review of `result.json` (`stopReason`, **`model_verified`**) and the agent's message.

## Procedure

1. **Choose workspace mode** (`isolated` default / `inplace` if requested) — see above.
2. **Stage cwd**
   - `isolated`: create worktree or `cp -Rc` clone; put task files inside it.
   - `inplace`: use the live path; write `task.txt` somewhere convenient (often the repo or `/tmp`).
3. **Pick agent + model** — default agent: OpenCode. Model list:
   [references/opencode.md](references/opencode.md).
4. **Run** — one command drives the whole ACP session:
   ```
   python <this-skill-dir>/scripts/acp_run.py --agent opencode --model <provider/model> \
     --cwd <workspace> --prompt-file <task.txt> --out <out-dir> --timeout 1200
   ```
   `<workspace>` is the sandbox path (`isolated`) or the live tree (`inplace`). The client resolves
   the agent command, sets the model, runs `initialize → session/new → session/prompt`, auto-approves
   permissions, and writes `agent_message.txt`, `tool_calls.txt`, and `result.json` to `--out`.
5. **Review** — `result.json`, agent message, and `git diff` in `--cwd`.
   - `isolated`: promote accepted edits into the live tree, then teardown.
   - `inplace`: keep or revert uncommitted edits as the user prefers.
6. **Teardown** — only for `isolated` (remove worktree / delete clone). Skip for `inplace`.

## Writing the task prompt

Put everything in the prompt and as files under `--cwd`: the task, constraints (no
install/network/commit unless allowed), and how to report results. To make the agent follow a
procedure (a skill, a checklist), stage those files under cwd and tell it to read them by path.

In **`inplace`**, state constraints extra clearly — there is no throwaway layer if the agent goes
wrong.

## Examples

- **Isolated second opinion:** "What would GLM-5.2 do about this failing test?" → `cp -Rc` or
  worktree, `acp_run.py … --cwd <clone> …`, review diff, promote or discard.
- **In-place fix:** "OpenCode でこのブランチの失敗テストをそのまま直して" → `--cwd` = live repo,
  review `git diff` there, leave uncommitted unless asked to commit.
- **Model comparison:** always **`isolated`**, two separate copies, same prompt, different
  `--model` (e.g. `qwen3.7-max` vs `deepseek-v4-pro`). Run **sequentially, not in parallel** —
  OpenCode serialises through one SQLite db (`database is locked`; see
  [references/opencode.md](references/opencode.md) § Gotchas).

## Troubleshooting

- **A model misreports its identity** (e.g. claims to be Claude). Ignore the prose; trust
  `result.json`'s `model_verified` (read back from the agent's own session record).
- **The agent didn't edit anything:** check `--cwd` is correct/writable and read
  `out/acp_stderr.log`; confirm auth (see references/opencode.md).
- **Hang or timeout:** raise `--timeout`; the last activity is in `out/acp_transcript.log`.
- **Unwanted live-tree edits:** you used `inplace` (or pointed `--cwd` at the live repo). Revert
  with `git checkout` / `git restore`; next time use `isolated`.

## Other agents

[references/agents.md](references/agents.md) is a catalog of ~39 ACP-compatible agents with their
launch commands (OpenCode, Gemini CLI `gemini --acp`, Qwen Code `qwen --acp`, Cursor `agent acp`,
Codex via `codex-acp`, Goose `goose acp`, Kimi `kimi acp`, and more), each with a confidence flag and
doc link. Treat it as a verified-as-of-research snapshot — re-check an entry before relying on it.

**Drive any of them right now** with `--cmd` (no code change): pick the launch command from the
catalog and pass it through —
`python <this-skill-dir>/scripts/acp_run.py --cmd "qwen --acp" --cwd <workspace> --prompt-file
<task.txt> --out <out>`. Model selection and verification are skipped on this path (they're
agent-specific), so set the model however that agent expects and confirm it yourself.

**Grok (xAI)** is the worked second example after OpenCode — verified end-to-end here on the
`--cmd` path. Its ACP server is a subcommand and its model goes on the command line:

```
python <this-skill-dir>/scripts/acp_run.py \
  --cmd "grok agent -m grok-composer-2.5-fast --always-approve stdio" \
  --cwd <workspace> --prompt-file <task.txt> --out <out>
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
