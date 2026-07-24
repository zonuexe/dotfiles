# Entry rewrite examples

Load this when a bullet is ambiguous or when teaching the release-style bar by
example. Prefer the main skill's short reject/prefer block for routine work.

**All good examples keep each prose block on one physical line** (paragraphs and
bullets). GitHub soft-wraps on render — do not hard-wrap for width. Bad examples
that hard-wrap are called out explicitly.

## No hard-wrap (paragraphs and lists)

```markdown
# ✗ column-wrapped release summary
This release speeds up incremental analysis on large apps and tightens several
diagnostics. It also removes a deprecated plugin hook.

# ✓ whole summary on one physical line
This release speeds up incremental analysis on large apps and tightens several diagnostics. It also removes a deprecated plugin hook.

# ✗ column-wrapped parent bullet
- Widget lookups no longer re-fetch on every call, which removes timeouts
  under load ([#12](https://github.com/org/repo/pull/12)).

# ✗ wrapped child under a one-line parent
- Widget lookups no longer re-fetch on every call, which removes timeouts under load.
  - Results are cached for the process lifetime; restart the process to pick
    up upstream changes.

# ✓ parent and child each one physical line (long is OK)
- Widget lookups no longer re-fetch on every call, which removes timeouts under load ([#12](https://github.com/org/repo/pull/12)).
  - Results are cached for the process lifetime; restart the process to pick up upstream changes.
```

## Collapse many commits into one user change

```markdown
# ✗ commit-style accumulation under [Unreleased]
- **[cache]** Wire analysis scope into dispatcher lookup.
- **[cache]** Walk RBS-only ancestors when the subclass is missing from the env.
- **[cache]** Add allow-list so ActionController::Base stays Dynamic[Top].
- **[tests]** Cover inherited call on RBS-only parent.

# ✓ one release-style entry (still one line per bullet)
- Inherited method calls on an allow-listed RBS-only parent are resolved for type checking, so a typo on a contract method is reported instead of ignored.
  - Ancestors outside the allow-list keep the previous untyped fallback so incomplete third-party signatures do not flood false positives.
```

## Split a glued merge artefact

```markdown
# ✗ two topics in one bullet
- CLI `--json` output is stable and docs mention the new `cache.validation` key.

# ✓ split by kind (Fixed vs Added) or at least by topic
- CLI `--json` output key order is stable across runs.
- Documented the `cache.validation` setting (`stat` default, or `digest`).
```

## Fix vs internal rename

```markdown
# ✗ implementer-facing
- Rename `GroupForBaseline` helper and make paths relative before hashing.

# ✓ user-facing (one line)
- Baseline grouping no longer treats the same file as two buckets when invoked with an absolute path versus a relative one.
```

## Em-dash run-on → bullet + child

```markdown
# ✗
- Plugins ship inside the main gem — `require "foo"` works with no RUBYLIB hack. Activate with `plugins: [foo]`.

# ✓
- Bundled plugins ship inside the main gem, so `require "foo"` works without a `RUBYLIB` or Gemfile workaround.
  - Activate a plugin with one line in config: `plugins: [foo]`.
```

## Links and credits

```markdown
# ✗ bare number only (dead in repo-rendered Markdown)
- Fixed crash on empty config (#170).

# ✗ link broken onto a second line
- Fixed a crash on empty config
  ([#170](https://github.com/org/repo/pull/170)).

# ✓ full markdown link on the same physical line as the sentence
- Fixed a crash on empty config ([#170](https://github.com/org/repo/pull/170), thank you [#166](https://github.com/org/repo/issues/166)!).
```

## Security and yank

```markdown
## [1.2.3] - 2026-01-15

### Security

- Patched path traversal in template include when the include root was unset ([#90](https://github.com/org/repo/pull/90)).

## [1.2.2] - 2026-01-10 [YANKED]

### Fixed

- …
```

Yanked versions **keep** their notes; the heading carries ` [YANKED]` so the
removal is loud and still parseable.
