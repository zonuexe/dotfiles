# OpenCode (ACP agent reference)

OpenCode is the default external agent for this skill. `scripts/acp_run.py --agent opencode`
handles all of the below automatically; this file is for when you need to understand or debug it.

## Command resolution

- ACP server: **`opencode acp`** (it speaks JSON-RPC over stdio). `acp_run.py` prefers a locally
  installed `opencode` on `PATH`, and otherwise falls back to **`npx -y opencode-ai@latest acp`**
  (slower first run — npx downloads the package).
- **Provenance:** OpenCode is listed in the ACP agent directory
  (<https://agentclientprotocol.com/get-started/agents>); the `opencode acp` command is from
  OpenCode's own ACP docs (<https://opencode.ai/docs/acp/>) and was confirmed here by a live ACP
  handshake. The directory does not publish launch commands, so this command must be kept current by
  hand against OpenCode's docs.
- The client passes `--cwd <workspace>`; OpenCode operates in that directory
  (isolated worktree/clone by default, or the live tree in in-place mode).

## Model selection

`opencode acp` has **no `--model` flag**. The model is taken from `opencode.json` in the cwd:

```json
{ "$schema": "https://opencode.ai/config.json", "model": "opencode-go/glm-5.2" }
```

`acp_run.py --model <id>` writes/merges this file for you. Model id format is `provider/model`; for
this user's subscription the provider is **`opencode-go`** (a bare id like `glm-5.2` is auto-prefixed
to `opencode-go/glm-5.2`).

### Available models (display name → id)

| Display | id (`provider/model`) |
|---|---|
| GLM-5.2 | `opencode-go/glm-5.2` |
| GLM-5.1 | `opencode-go/glm-5.1` |
| Kimi K2.7 Code | `opencode-go/kimi-k2.7-code` |
| Kimi K2.6 | `opencode-go/kimi-k2.6` |
| MiMo-V2.5-Pro | `opencode-go/mimo-v2.5-pro` |
| MiMo-V2.5 | `opencode-go/mimo-v2.5` |
| Qwen3.7 Max | `opencode-go/qwen3.7-max` |
| Qwen3.7 Plus | `opencode-go/qwen3.7-plus` |
| Qwen3.6 Plus | `opencode-go/qwen3.6-plus` |
| MiniMax M3 | `opencode-go/minimax-m3` |
| MiniMax M2.7 | `opencode-go/minimax-m2.7` |
| DeepSeek V4 Pro | `opencode-go/deepseek-v4-pro` |
| DeepSeek V4 Flash | `opencode-go/deepseek-v4-flash` |

Refresh this list with `opencode models` (or `npx -y opencode-ai@latest models`); model ids change
over time, so treat the table as a snapshot.

## Verifying which model actually ran

A model's self-report in prose is unreliable (some claim to be Claude). The authoritative source is
OpenCode's own session record:

```
opencode export <sessionId>        # JSON; look for "modelID" and "providerID"
```

`acp_run.py` does this after each run and records it as `model_verified` in `result.json`.

## Auth

These models need the OpenCode subscription. Log in once with `opencode auth login` (or
`opencode auth`). When already logged in, the ACP `initialize` response may still list an
`opencode-login` auth method, but sessions work via the stored credentials — the client notes this
and proceeds. If sessions fail with auth errors, re-run `opencode auth login` in a terminal.

## Gotchas

- **No concurrent sessions — run them SEQUENTIALLY.** OpenCode serialises everything through one
  SQLite database (`~/.local/share/opencode/opencode.db`). Two or more `opencode acp` processes
  running at once contend for its write lock; the losers fail at the ACP handshake with
  `Error: Unexpected error` / `database is locked`, which surfaces to the client as
  `timeout waiting for initialize` (the run records `"ok": false`, `"model_verified": null`,
  `elapsed_s` ~0). Observed 2026-06-20 on a 13-model fan-out: a batch of **6 parallel** runs lost
  **5** to the lock; re-running those same 5 **one-at-a-time succeeded every time** (not a model
  problem). So a multi-model comparison must loop the models with one `acp_run.py` at a time — never
  parallel `&`/background batches. (The lock is on OpenCode's own db; it is unrelated to the
  per-run sandboxes, which can safely be distinct dirs.)
- **npx latency:** the first `npx -y opencode-ai@latest …` call downloads OpenCode; later calls are
  cached. Installing `opencode` locally avoids this.
- **Permissions:** OpenCode requests permission for edits/bash via ACP `session/request_permission`;
  the client auto-approves. Prefer an isolated `--cwd`; in-place mode edits the live tree.
- **Session artifacts:** OpenCode may write `opencode.json` (the model file you set), a `.opencode/`
  dir, etc. into the cwd. Strip these before diffing/grading if you want only the agent's code edits.
  In in-place mode those files appear in the user's repo — remove or gitignore as appropriate.
