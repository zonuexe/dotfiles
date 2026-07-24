# Seal workflow (release cut)

Load this when sealing `[Unreleased]` into a versioned section. Mid-cycle
appends only need [entry quality](entry-examples.md) + a single bullet under
`[Unreleased]`.

## 1. Gather the cycle's changes

- Read the whole `[Unreleased]` block (and any draft notes the user provided).
- Take the previous release date from the latest `## [x.y.z] - DATE` heading.
- List merged work since that date so linking and completeness are lookups.
  On GitHub:

  ```sh
  gh pr list --state merged --limit 200 --search "merged:>=<prev-release-date>" \
    --json number,title,mergedAt --jq 'sort_by(.mergedAt) | .[] | "#\(.number)  \(.title)"'
  ```

  A PR on the list with no entry is either a missed user-facing change or
  correctly internal — decide which; do not skip past it silently.
- If `[Unreleased]` is empty, build entries from the PR list / commits using
  entry-quality rules (still not a raw log dump).

## 2. Rewrite every bullet

For **each** top-level bullet, classify: release-style (leave) or commit-style
(rewrite). Then:

1. Lead sentence → one clause; move how/why into child items; delete internal detail.
2. Link the landing PR (full markdown form).
3. Split merge artefacts (two changes glued into one bullet).
4. Assign each bullet to Added / Changed / Deprecated / Removed / Fixed / Security.
5. Re-read as a user. Any top-level bullet with two sentences or an em-dash
   clause is unfinished. Every prose block must still be **one physical line**.

## 3. Open the version section

Immediately under `[Unreleased]`:

```markdown
## [x.y.z] - YYYY-MM-DD
```

Use today's date unless the user specifies another. Keep `[Unreleased]` empty
above it (ready for the next cycle).

## 4. Release summary (when the project uses one)

If recent versions open with a short prose paragraph **before the first `###`**,
write one (≈3–4 simple sentences):

- Lead with the dominant theme, name secondary threads, close with "also fixes …" if needed.
- Themes, not a recap of every bullet.
- Same user-facing bar; link sparingly using the file's existing link style.
- **One physical line for the whole paragraph.**

If the project has never used summaries, do not start unless the user asks.

## 5. Place categories and bullets

Move sealed bullets under the new version heading, grouped by type.
Omit empty `###` categories. No `####` under a version block.

## 6. Update footer compare links

```markdown
[Unreleased]: https://github.com/org/repo/compare/vx.y.z...HEAD
[x.y.z]: https://github.com/org/repo/compare/v(prev)...vx.y.z
```

- Match existing host, path, and tag prefix (`v1.2.3` vs `1.2.3`).
- Do not invent a host the file never used.

## 7. Done criteria

- `[Unreleased]` empty (or only deliberately deferred items)
- New `## [x.y.z] - YYYY-MM-DD` with correct categories
- Top-level bullets: one sentence, user-facing, no em-dash run-ons
- No hard-wrapped prose (one physical line per bullet and summary paragraph)
- Full markdown PR links where a PR exists
- Footer compare links updated
- Language and bullet style match the rest of the file

## Checklist

- [ ] Conventions read from the existing file
- [ ] Cycle PRs/commits surveyed for completeness
- [ ] Every former `[Unreleased]` bullet classified / rewritten if needed
- [ ] No hard-wrapped prose
- [ ] Categories correct; empty ones omitted
- [ ] Version heading + date; optional summary if the project uses them
- [ ] Footer compare links updated
- [ ] Final read-through as a user who never saw the commits
