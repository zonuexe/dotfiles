---
name: keep-a-changelog
description: >
  Write and seal release notes in Keep a Changelog 1.1.0 form without hard-wrapped
  prose (GitHub soft-wraps bullets and paragraphs on render). Use whenever the
  user asks to update CHANGELOG.md, seal or cut [Unreleased] into a versioned
  section, write a release summary, refresh compare links, turn commit-style
  notes into user-facing bullets, or prepare changelog content for a GitHub
  Release body — even if they only say "write the changelog" or "what's in this
  release" without naming Keep a Changelog.
---

# Keep a Changelog (release notes)

Turn accumulated notes into a **human-readable** version section. The
deliverable is prose quality and structure — not a dump of the git log.

Follow [Keep a Changelog 1.1.0](https://keepachangelog.com/en/1.1.0/). Prefer
[Semantic Versioning](https://semver.org/) for version numbers when the project
already does.

## When this skill applies

| Ask | Do |
|-----|-----|
| Cut / seal a release's notes | Full workflow below |
| Polish `[Unreleased]` only | Entry quality + consolidation; leave version heading alone |
| Add one PR/commit note mid-cycle | Append under `[Unreleased]` in release style (do not invent a version) |
| "Write release notes from git log" | Research → draft `[Unreleased]` or the new version section in this form |

Out of scope: bumping package version files, tags, publishing, CI gates. Those
belong to a project release skill; this skill owns only `CHANGELOG*` content.

## Discover project conventions first

Before rewriting, read the existing `CHANGELOG.md` (and any archive pointer at
the top) and match what is already there:

- **Language** of the file (do not switch languages mid-file)
- **Filename** (`CHANGELOG.md` is the Keep a Changelog default; some repos use
  `CHANGELOG`, `HISTORY`, `NEWS`)
- **Version heading shape**: `## [x.y.z] - YYYY-MM-DD` (ISO-8601 date)
- **Category headings** in use (see Types of changes)
- **Bullet style**: subsystem labels (`**[area]** …`), plain bullets, PR link
  placement, thank-you lines
- **Footer compare links**: host and path pattern (`github.com/org/repo/compare/…`)
- **Release summary**: some projects put a short prose paragraph under the
  version heading before the first `###`; keep doing so if present

If the file is missing, scaffold a minimal Keep a Changelog skeleton (title,
intro line linking the format + SemVer if appropriate, empty `## [Unreleased]`,
footer ready for compare links) and then fill the release section.

## No hard-wrap: prose is one physical line per block

GitHub (and most Markdown viewers) **soft-wrap** text to the viewport. Hard
newlines inside a paragraph or list item do not improve the rendered page; they
only shape the source. So in `CHANGELOG.md`, **do not hard-wrap prose**.

Applies to **all** body text, not only list items:

| Block | One physical line means |
|-------|-------------------------|
| Top-level bullet (`- …`) | Whole item on one line |
| Child bullet (`  - …`) | Whole item on one line |
| Release-summary paragraph | Whole paragraph on one line (several sentences OK) |
| File intro / archive blurb | Whole paragraph on one line |

Newlines are for **structure only**: blank line between blocks, a new heading,
a new list item. Never break mid-sentence or at column 80/100/120.

- **Long is fine.** Dense release notes routinely run hundreds of characters on
  one line; GitHub will wrap them on render.
- **Readable structure ≠ line width.** Split ideas with a new bullet or a new
  paragraph, not with a soft line break inside the same block.
- If an editor, Prettier, or another formatter reflows a line, put each block
  back to a single physical line before finishing.

```markdown
# ✗ hard-wrapped paragraph (column wrap)
This release speeds up incremental analysis on large apps and tightens several
diagnostics. It also removes a deprecated plugin hook.

# ✓ same paragraph, one physical line (GitHub soft-wraps on render)
This release speeds up incremental analysis on large apps and tightens several diagnostics. It also removes a deprecated plugin hook.

# ✗ hard-wrapped bullet
- Widget lookups no longer re-fetch on every call, which removes timeouts
  under load ([#12](https://github.com/org/repo/pull/12)).

# ✓ one physical line
- Widget lookups no longer re-fetch on every call, which removes timeouts under load ([#12](https://github.com/org/repo/pull/12)).
```

Why this matters: hard wraps look neat in a narrow buffer but produce noisy
diffs, force reflow on every edit, and make “new block vs continuation” ambiguous
in the source. Trust the renderer for line length.

## Entry quality

Changelogs are for **users and integrators**, not implementers. Each top-level
bullet should answer "what can I do / what broke / what should I notice?"

### Other rules

1. **One sentence per top-level bullet.** One period. No em-dash run-ons, no
   multi-clause commit essays. Extra "how / why / caveats" go in indented
   child items (`  - …`), a few sentences max, one topic each — each child
   still one physical line (see **No hard-wrap** above).
2. **User-facing only.** Drop class renames, test-only work, pure refactors,
   coverage counts, and plumbing nobody sees unless the user must act (migrate,
   reconfigure, stop relying on something). Test: *would someone care if they
   never opened the source?* If no, delete the entry.
3. **Not a commit log.** Many commits may collapse into one entry; one fat
   line may split into several. Reorder so related changes sit together under
   the right category.
4. **Prefer full markdown links for issues/PRs**, not bare `#123`:

   ```markdown
   ([#170](https://github.com/org/repo/pull/170))
   ```

   Bare `#170` may autolink in a GitHub Release body but stays dead text in
   `CHANGELOG.md` rendered from the tree. The full form works in both. Put the
   link at the end of the bullet; when one bullet consolidates several PRs,
   hang each link on the child item it belongs to. Omit the link when there
   was no PR (docs-only direct commits, etc.) — do not invent one.
5. **Credit reporters** when the project already does (`thank you @handle!`)
   and link the reporting issue if useful.
6. **Match existing label prefixes** if the project uses them
   (`**[cli]**`, `**[api]**`, …). Do not invent a labelling scheme on a plain
   bullet file.

Reject on sight (rewrite in place):

```markdown
# ✗ two sentences joined by an em-dash
- Widgets now cache results — previously every call hit the network and timed out under load.

# ✗ internal implementation detail
  - Refactored `WidgetCache` to use a concurrent hash map.

# ✗ commit-message prose
- Fix `normalize_path` to strip trailing slashes before building cache keys.

# ✗ hard-wrapped parent or child (or hard-wrapped summary prose)
- Fixed a crash when widget paths ended with a trailing slash and the cache
  key builder assumed a relative path.
```

Prefer (each bullet / paragraph is one physical line):

```markdown
- Widget lookups no longer re-fetch on every call, which removes timeouts under load.
  - Results are cached for the process lifetime; restart the process to pick up upstream changes.

- Fixed a crash when widget paths ended with a trailing slash.
```

More before/after shapes: [references/entry-examples.md](references/entry-examples.md).

## Types of changes

Use Keep a Changelog headings **verbatim** (no `### Added — feature X`, no
`####` under a version block):

| Heading | For |
|---------|-----|
| `### Added` | New features |
| `### Changed` | Changes in existing behaviour |
| `### Deprecated` | Soon-to-be removed |
| `### Removed` | Now removed |
| `### Fixed` | Bug fixes |
| `### Security` | Vulnerabilities |

- Group the same kind of change under the same heading.
- **Omit empty categories** — missing sections mean "nothing of that kind",
  not a forgotten heading.
- Yanked releases keep the notes and append ` [YANKED]` on the version
  heading: `## [0.0.5] - 2014-12-13 [YANKED]`.

## Workflow: seal a release

Do not skip step 2. Mechanical heading moves with unreviewed commit-style
bullets produce a release that is not done.

### 1. Gather the cycle's changes

- Read the whole `[Unreleased]` block (and any draft notes the user provided).
- Take the previous release date from the latest `## [x.y.z] - DATE` heading.
- List merged work since that date so linking and completeness are lookups,
  not memory. On GitHub:

  ```sh
  gh pr list --state merged --limit 200 --search "merged:>=<prev-release-date>" \
    --json number,title,mergedAt --jq 'sort_by(.mergedAt) | .[] | "#\(.number)  \(.title)"'
  ```

  A PR on the list with no entry is either a missed user-facing change or
  correctly internal — decide which; do not skip past it silently.
- If `[Unreleased]` is empty, build entries from the PR list / commits using
  the entry-quality rules (still not a raw log dump).

### 2. Rewrite every bullet (mandatory)

For **each** top-level bullet, classify: release-style (leave) or commit-style
(rewrite). Then:

1. Lead sentence → one clause; move how/why into child items; delete internal
   detail.
2. Link the landing PR from the step-1 list (full markdown form).
3. Split **merge artefacts** (two changes glued into one bullet, a second
   topic mid-sentence).
4. Assign each bullet to Added / Changed / Deprecated / Removed / Fixed /
   Security.
5. Re-read the sealed block top-to-bottom as a user. Any top-level bullet with
   two sentences or an em-dash clause is unfinished. Confirm every prose block
   (summary paragraph, parent bullets, child bullets) is still **one physical
   line** — no accidental hard wrap from the rewrite.

### 3. Open the version section

Immediately under `[Unreleased]`, add:

```markdown
## [x.y.z] - YYYY-MM-DD
```

Use today's date unless the user specifies another. Keep `[Unreleased]` as an
empty section above it (ready for the next cycle).

### 4. Release summary (when the project uses one)

If recent versions open with a short prose paragraph **before the first
`###`**, write one for this release too (≈3–4 simple sentences):

- Lead with the dominant theme, name one or two secondary threads, close with
  a brief "also fixes …" clause if needed.
- Themes, not a recap of every bullet.
- Same user-facing bar as the bullets; link sparingly to driving design docs
  using the file's existing link style.
- **One physical line for the whole paragraph.** Do not hard-wrap the summary;
  GitHub will soft-wrap it on the rendered page and in the Release body.

If the project has never used summaries, do not start unless the user asks.

### 5. Place the categories and bullets

Move the sealed bullets under the new version heading, grouped by type.
Leave `[Unreleased]` empty (or with only notes that truly stay unreleased).

### 6. Update footer compare links

At the bottom of the file, Keep a Changelog uses reference-style links:

```markdown
[Unreleased]: https://github.com/org/repo/compare/vx.y.z...HEAD
[x.y.z]: https://github.com/org/repo/compare/v(prev)...vx.y.z
[prev]: …
```

- Point `[Unreleased]` at `compare/<new-tag>...HEAD` (match the repo's tag
  prefix: `v1.2.3` vs `1.2.3`).
- Add `[x.y.z]` comparing previous tag → new tag.
- Infer `org/repo` and tag style from existing footer links; do not invent a
  host the file never used.
- Version headings stay linkable via these definitions (`## [x.y.z]` →
  `[x.y.z]: …`).

### 7. Done criteria

- `[Unreleased]` is empty (or only deliberately deferred items).
- New `## [x.y.z] - YYYY-MM-DD` sits directly below it with correct categories.
- Every top-level bullet is one sentence, user-facing, free of em-dash run-ons.
- **No hard-wrapped prose**: each bullet and each summary/intro paragraph is
  one physical line (GitHub soft-wraps on render).
- PR links use full markdown form where a PR exists; no bare `#N` as the only
  reference.
- Footer `[Unreleased]` and `[x.y.z]` compare links resolve to the right range.
- No empty `###` sections; no `####` under the version block.
- Language and bullet style match the rest of the file.

## Mid-cycle: appending to `[Unreleased]`

When landing work before a release:

- Write the entry **release-style at land time** so seal is mostly
  consolidation, not a wholesale rewrite.
- Put it under the right `###` category inside `[Unreleased]` (create the
  category heading if missing).
- Still one sentence + optional child detail; still full PR link when known;
  still **one physical line** per prose block (no hard-wrap).

## Checklist

- [ ] Conventions read from the existing file (language, labels, link style)
- [ ] Cycle PRs/commits surveyed for completeness
- [ ] Every former `[Unreleased]` bullet classified and rewritten if needed
- [ ] No hard-wrapped prose: bullets and summary/intro paragraphs are each one physical line
- [ ] Categories correct; empty ones omitted
- [ ] Version heading + date; optional summary if the project uses them
- [ ] Footer compare links updated for Unreleased + new version
- [ ] Final read-through as a user who never saw the commits
