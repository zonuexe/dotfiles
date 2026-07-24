# Workspace modes (inplace vs isolated)

The client auto-approves the external agent's file edits and shell commands.
Choose mode by **why** you are calling the model.

| Intent | Mode | `--cwd` | Goal |
|--------|------|---------|------|
| **Delegate work** — fix tests, review, implement via OpenCode/GLM/… | **`inplace`** | Live working tree | Edits land where the user works |
| **Verify a skill/procedure** — "can OpenCode Go complete this SKILL?" | **`isolated`** | `git worktree` or `cp -Rc` clone | Measure completion without polluting the live branch |

Also use **`isolated`** for multi-model comparison and discardable experiments.

## How to choose

1. Skill / procedure evaluation → **`isolated`**. Stage the skill + fixtures into the
   worktree; the agent must succeed from cwd + prompt alone.
2. Ordinary delegation → **`inplace`**.
3. User forces **worktree / 隔離** or **in place / そのまま** → honor that.
4. Ambiguous → ask once: *検証（worktree）か、作業委任（inplace）か?*

Never use `inplace` for skill-verification runs or multi-model fan-out.

## `inplace` — delegate real work

- `--cwd` = session repo root (or the path the user named).
- Put constraints in the task prompt: no installs/network/destructive ops/commits
  unless allowed.
- Review with `git diff` / `git status`; leave uncommitted unless asked to commit.
- OpenCode may write `opencode.json` / `.opencode/` into `--cwd`.

## `isolated` — verify skill completion

1. Disposable copy:
   - `git worktree add <path> -b acp/<slug>` — light; **omits** gitignored deps.
   - `cp -Rc <repo> <sandbox>` — keeps `vendor/` / `node_modules/` for build/test.
2. Stage `SKILL.md`, `references/`, scripts, fixtures into the copy. Tell the model
   to read those paths (no Claude skill loader).
3. Run `acp_run.py` with `--cwd` = that copy.
4. Judge success from `stopReason`, agent message, and skill done-criteria in the tree.
5. Usually **do not promote** — product is pass/fail. Teardown worktree/clone after.

## Task prompts

**Skill verification:** name the skill path, what "done" means, require a summary of
what passed/failed. Do not assume Claude skill conventions.

**Delegation:** usual coding handoff; constraints matter more (no throwaway layer).

## Examples

- Delegate: "OpenCode でこのブランチの失敗テストを直して" → inplace, live `--cwd`.
- Verify: "keep-a-changelog を glm-5.2 で完遂できるか" → worktree + staged skill,
  discard unless user wants edits.
- Compare models: always isolated, separate copies, **sequential** OpenCode runs
  (SQLite lock — see [opencode.md](opencode.md) § Gotchas).
