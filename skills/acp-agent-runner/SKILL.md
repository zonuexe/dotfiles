---
name: acp-agent-runner
description: >-
  Use this skill whenever the user wants a coding task carried out by an AI coding agent or model
  that is NOT Claude — for example OpenCode, GLM, Qwen, DeepSeek, Kimi, MiniMax, "another coding
  agent", "another vendor's model", or "whatever external agent is set up". Typical intents: hand
  or throw a task to that other agent and bring back its result; ask it to review changes, fix
  failing tests, or refactor; route work "through opencode"; or run the same job on two outside
  models to compare their outputs. This skill runs that external agent for you (over the Agent
  Client Protocol). Two main workspace modes: **inplace** for ordinary work handoff, **isolated**
  (worktree) for verifying that a skill/procedure can be completed by an external model (e.g.
  OpenCode Go). It applies in any language (English, 日本語, など) and to vague or indirect
  phrasing. Do NOT use it for Claude's own subagents (use the Agent tool instead), for plain
  Anthropic/LLM API scripts, or for running Claude itself — the defining signal is that the work
  goes to a different, non-Claude agent.
---

# Drive an external agent over ACP (safely)

Hand a self-contained task to an **external** agent (a different vendor's CLI/model) and get the
result back by speaking ACP — JSON-RPC over stdio — to it. This is **one-shot delegation, not a true
subagent**: there's no shared task list and no streaming into your own reasoning loop. Reach for it
when the value is *a different vendor's model* (real work handoff, skill verification, comparison),
since Claude's own Agent tool can only run Claude.

## Two product intents → two workspace modes

The client **auto-approves** the external agent's file edits and shell commands. Choose mode by
**why** you are calling the model, not only by wording:

| Intent | Mode | `--cwd` | Goal |
|--------|------|---------|------|
| **Delegate work** — "OpenCode にこの作業をやらせて" / fix tests / review / implement | **`inplace`** | Live working tree | Edits land where the user is already working |
| **Verify a skill/procedure** — "この SKILL を OpenCode Go で完遂できるか" / eval a checklist against an outside model | **`isolated`** | Disposable `git worktree` or `cp -Rc` clone | Measure completion without polluting the live branch; easy discard |

Also use **`isolated`** for multi-model comparison and any run you expect to throw away.

### How to choose

1. **Skill / procedure evaluation** (can model X complete skill Y? run this eval on GLM? dry-run a
   checklist with OpenCode Go?) → **`isolated`**. Stage the skill (and any fixtures) into the
   worktree; the agent must succeed from cwd + prompt alone.
2. **Ordinary delegation** (just do the coding/review task with an outside model) → **`inplace`**.
3. User forces the mode with **worktree / sandbox / 隔離** or **in place / そのまま / このブランチ**
   → honor that.
4. If still ambiguous → ask once: *検証（worktree）か、作業委任（inplace）か?* Prefer **`inplace`**
   only when the ask is clearly "do this work"; prefer **`isolated`** when the ask is about
   whether a skill/model combination works.

Do **not** use `inplace` for skill-verification runs or multi-model fan-out.

### Mode: `inplace` — delegate real work

- `--cwd` = the path the user wants edited (usually the session's repo root).
- Auto-approve writes/runs shell **in that tree** — put tight constraints in the task prompt
  (no installs/network/destructive ops/commits unless the user allowed them).
- Review with `git diff` / `git status` there; leave uncommitted unless asked to commit.
- OpenCode may drop `opencode.json` / `.opencode/` into `--cwd` — strip or leave per preference.

### Mode: `isolated` — verify skill completion (or discardable runs)

Use when the question is **"can this external model finish this procedure?"**, not "please ship
this change."

1. Create a disposable copy:
   - `git worktree add <path> -b acp/<task-slug>` (or detached) — light; **omits** gitignored
     deps (`vendor/`, `node_modules/`).
   - `cp -Rc <repo> <sandbox>` — heavier; **keeps** deps so build/test works.
2. **Stage what the skill needs inside the copy** — the `SKILL.md` (and `references/`, scripts,
   fixtures). The external agent has no access to Claude's skill loader; tell it in the prompt to
   read those paths and follow them.
3. `--cwd` = that copy. Run `acp_run.py`. Judge success from `stopReason`, agent message, and
   whether the skill's done criteria hold in the worktree (tests green, files produced, etc.).
4. **Usually do not promote** — the product is the verification result (pass/fail + notes). Promote
   only if the user asks to take the edits.
5. Teardown: `git worktree remove` or delete the clone.

Same constraints as inplace on installs/network/commits unless the verification scenario requires
otherwise (state that in the prompt).

### Common rules (both modes)

- The agent sees only **cwd + your prompt** (no chat memory, no Claude skills unless staged).
- Always review `result.json` (`stopReason`, **`model_verified`**) and the agent message.

## Procedure

1. **Classify intent** → `inplace` (delegate) or `isolated` (verify skill / discardable).
2. **Stage cwd** as above (live tree vs worktree + staged skill files).
3. **Pick agent + model** — default OpenCode; models:
   [references/opencode.md](references/opencode.md).
4. **Run**:
   ```
   python <this-skill-dir>/scripts/acp_run.py --agent opencode --model <provider/model> \
     --cwd <workspace> --prompt-file <task.txt> --out <out-dir> --timeout 1200
   ```
5. **Review**
   - `inplace`: `git diff` in the live tree; keep or revert.
   - `isolated` (skill verify): report whether the model completed the skill; quote evidence;
     teardown unless the user wants the edits.
6. **Teardown** only for `isolated`.

## Writing the task prompt

Put the task, constraints, and reporting format under `--cwd` / in the prompt.

**Skill verification:** explicitly name the skill path in the worktree, order of steps if needed,
and what "done" means (e.g. "follow SKILL.md end-to-end; stop when the checklist is satisfied;
summarize what you did and what failed"). Do not assume the model knows Claude skill conventions.

**Delegation:** same as usual coding handoff; constraints matter more because there is no
throwaway layer.

## Examples

- **Delegate (inplace):** "OpenCode でこのブランチの失敗テストを直して" → `--cwd` = live repo,
  review diff there.
- **Verify skill (isolated):** "keep-a-changelog を OpenCode Go の glm-5.2 で完遂できるか見たい"
  → worktree, copy/link the skill into it, prompt "Read skills/keep-a-changelog/SKILL.md and seal
  this fixture CHANGELOG…", judge pass/fail from outcomes, usually discard the tree.
- **Compare models:** always **`isolated`**, separate copies, sequential runs (OpenCode SQLite
  lock — [references/opencode.md](references/opencode.md) § Gotchas).

## Troubleshooting

- **A model misreports its identity** (e.g. claims to be Claude). Ignore the prose; trust
  `result.json`'s `model_verified` (read back from the agent's own session record).
- **The agent didn't edit anything:** check `--cwd` is correct/writable and read
  `out/acp_stderr.log`; confirm auth (see references/opencode.md).
- **Skill verify fails because files are missing:** the skill was not staged into the worktree, or
  the prompt did not tell the model to open it.
- **Hang or timeout:** raise `--timeout`; the last activity is in `out/acp_transcript.log`.
- **Unwanted live-tree edits:** you used `inplace` (or pointed `--cwd` at the live repo). Revert
  with `git checkout` / `git restore`; use `isolated` for verification runs.
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
