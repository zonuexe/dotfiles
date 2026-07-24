# Grok CLI (xAI) — ACP agent reference

Grok is xAI's coding CLI. It is a **separate program from OpenCode** — its own binary,
its own auth, its own session store — but it speaks ACP, so `acp_run.py` drives it through
the same harness via `--cmd`. This file is the Grok counterpart of `opencode.md`.

- Homepage / install: <https://x.ai/cli>
- Headless + ACP docs: <https://docs.x.ai/build/cli/headless-scripting#acp>

## Command resolution

Grok's ACP server is a **subcommand**, not a `--acp` flag:

```
grok agent stdio
```

The official docs say this verbatim: *"This runs Grok as an ACP agent over JSON-RPC on
stdin/stdout."* Confirmed here by a live ACP handshake (`initialize → session/new →
session/prompt` all answered). Note the two other `grok agent` transports are **not** ACP:
`grok agent headless` runs over xAI's WebSocket relay, `grok agent serve` is a WS server —
use `stdio`.

- **Provenance:** Grok CLI is xAI's own tool; the `grok agent stdio` command is from its
  headless-scripting docs (link above) and was verified live. Keep it current by hand
  against those docs — CLIs change.
- The local binary in this environment is `~/.grok/bin/grok` (`grok --version` to confirm
  it is on `PATH`). `mise which rigor`-style shims do not apply; Grok is installed by its
  own installer.

## Model selection

Unlike OpenCode (which reads `opencode.json`), Grok takes the model on the **command line**
via `-m` / `--model`, which lives on the `grok agent` parent (put it before the `stdio`
subcommand):

```
grok agent -m grok-composer-2.5-fast --always-approve stdio
```

Pass this whole string to `acp_run.py --cmd "..."`. `--always-approve` is Grok's
auto-approve flag (the ACP equivalent of OpenCode's per-request auto-approval) — it is what
makes unattended runs work, and is **safe only because the cwd is a sandbox** (same rule as
every agent here).

### Model ids

List them live — ids drift:

```
grok models
```

Observed set (snapshot, verify before relying on it):

| Display | id (`-m` value) |
|---|---|
| Grok 4.5 (default) | `grok-4.5` |
| Grok Composer 2.5 Fast | `grok-composer-2.5-fast` |

There is no bare `grok-composer-2.5` id in the CLI — the Composer 2.5 model is exposed as
`grok-composer-2.5-fast`. `grok models` also prints the account you are logged in as and the
default model.

## Verifying which model actually ran

**There is no programmatic model-verification for Grok** the way OpenCode has
(`opencode export <sessionId>` → `modelID`). Grok's own inspection commands are
Markdown-only:

- `grok export <SESSION_ID>` writes a **Markdown** transcript (no `modelID` / `providerID`
  field) — `--format json` is not offered.
- `grok sessions {list,search,delete}` lists sessions but not the per-session model.

So on the `--cmd` path `acp_run.py` records `model_verified: null` for Grok. Trust instead:
(1) the `-m` id you passed on the launch command, and (2) `grok models` confirming that id
exists for your account. State in any comparison writeup that Grok's model is asserted from
the launch flag, not read back from a session record.

## Auth

Grok authenticates against **grok.com / xAI**, independent of OpenCode:

- `grok login` (interactive), or set `XAI_API_KEY` in the environment.
- When already logged in, the ACP `initialize` response advertises auth methods
  (`cached_token`, `grok.com`) and sessions proceed on the stored credential — the client
  notes this and continues (same shape as OpenCode's `opencode-login`).
- If sessions fail with an auth error, re-run `grok login` in a terminal (or check
  `XAI_API_KEY`).

## The non-ACP headless path (when you don't need ACP)

For a one-shot scripted prompt with structured output — no ACP, no `acp_run.py` — Grok has a
native headless mode:

```
grok -p "…prompt…" -m grok-composer-2.5-fast --output-format json      # one JSON object at the end
grok -p "…prompt…" --output-format streaming-json                       # newline-delimited events
```

Use the ACP path (`grok agent stdio` via `acp_run.py`) when you want the same harness,
artifacts (`agent_message.txt` / `tool_calls.txt` / `result.json`), and auto-approved
tool-driven editing as the other agents; use `grok -p … --output-format json` when you only
need a single model answer and want to parse it directly.

## Gotchas

- **Model flag placement:** `-m` is a `grok agent` option, so it must come **before** the
  `stdio` subcommand — `grok agent -m <id> --always-approve stdio`, not
  `grok agent stdio -m <id>` (the latter is parsed as `stdio`'s option and rejected).
- **Separate session db from OpenCode** — Grok and OpenCode do not share the SQLite lock
  that forces OpenCode runs to be sequential, so a Grok run *can* overlap an OpenCode run.
  For a clean, reviewable comparison it is still simplest to run everything serially.
- **`--always-approve` is unconditional** — it approves every tool call including shell and
  file writes. Never point a Grok ACP run with `--always-approve` at anything but a
  throwaway sandbox cwd.
- **Session artifacts:** Grok writes under `~/.grok/` (sessions, config), not into the cwd,
  so it leaves the sandbox cleaner than OpenCode (which drops `opencode.json` / `.opencode/`
  into the cwd).
