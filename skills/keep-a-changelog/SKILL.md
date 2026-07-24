---
name: keep-a-changelog
description: >
  Seal Keep a Changelog notes without hard-wrapped prose (GitHub soft-wraps).
  **UTILITY SKILL**. USE FOR: update CHANGELOG.md; seal or cut [Unreleased];
  release summary; refresh compare links; rewrite commit-style notes; GitHub
  Release body; "what's in this release". DO NOT USE FOR: version bumps/tags;
  publishing packages; CI gates; raw git-log as final notes; marketing posts.
  FOR SINGLE OPERATIONS: mid-cycle [Unreleased] bullet or full seal.
metadata:
  version: "1.0"
license: MIT
---

# Keep a Changelog

Human-readable version sections — not a git log (Keep a Changelog 1.1.0).

## Scope

- **Seal a release** → [references/seal-workflow.md](references/seal-workflow.md)
- **Polish `[Unreleased]`** → entry quality only
- **Mid-cycle note** → one release-style bullet under `[Unreleased]`

Changelog content only (not bumps, tags, publish, or CI).

## Rules

1. **Match** existing language, headings, labels, PR-link style, footer, summary habit.
2. **No hard-wrap** — one physical line per bullet/child/summary (GitHub soft-wraps).
3. **User-facing** — one sentence per top-level bullet; drop internal-only work.
4. **Full markdown PR links**, not bare `#N`.
5. **Categories** verbatim: Added, Changed, Deprecated, Removed, Fixed, Security (omit empty).

Examples: [references/entry-examples.md](references/entry-examples.md).

## Seal (summary)

Gather notes + PRs → rewrite → `## [x.y.z] - YYYY-MM-DD` under empty
`[Unreleased]` → optional one-line summary → footer compare links. Details in
[references/seal-workflow.md](references/seal-workflow.md).

## Mid-cycle

Category + one sentence + optional child + full PR link + one line. No new version heading.
