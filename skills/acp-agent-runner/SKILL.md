---
name: acp-agent-runner
description: >
  Drive a non-Claude coding agent over ACP (auto-approved tools). **UTILITY SKILL**.
  USE FOR: hand work to OpenCode/GLM/Qwen/DeepSeek/Kimi; "another agent"; external
  review or test fix; compare outside models; verify a SKILL via OpenCode Go;
  inplace delegate vs worktree skill eval. DO NOT USE FOR: Claude subagents (Agent
  tool); plain LLM API scripts; running Claude itself. FOR SINGLE OPERATIONS: one
  acp_run.py session per model/workspace.
metadata:
  version: "1.1"
license: MIT
---

# Drive an external agent over ACP

One-shot handoff to a **different vendor's** agent via ACP — not a Claude subagent.

## Mode

| Intent | Mode | `--cwd` |
|--------|------|---------|
| Delegate work | **`inplace`** | Live tree |
| Verify a skill | **`isolated`** | Worktree / `cp -Rc` |

Rules: [references/workspace-modes.md](references/workspace-modes.md).

## Procedure

1. Classify intent → mode.
2. Stage `--cwd` (skill verify: copy skill files into the worktree).
3. Models: [references/opencode.md](references/opencode.md).
4. Run:
   ```
   python <this-skill-dir>/scripts/acp_run.py --agent opencode --model <provider/model> \
     --cwd <workspace> --prompt-file <task.txt> --out <out-dir> --timeout 1200
   ```
5. Review `result.json` (`stopReason`, **`model_verified`**), message, `git diff`.
6. Teardown only for `isolated` (skill verify usually does not promote).

## Prompt

Task + constraints + done criteria under `--cwd`. Skill verify: name staged
`SKILL.md`. External agent has no Claude skill loader.

## Other agents

[agents.md](references/agents.md) ·
[grok.md](references/grok.md)

## Troubleshooting

Trust `model_verified` over prose. No edits → `--cwd`, `acp_stderr.log`, auth.
Missing skill files → not staged. Hang → raise `--timeout` / `acp_transcript.log`.
