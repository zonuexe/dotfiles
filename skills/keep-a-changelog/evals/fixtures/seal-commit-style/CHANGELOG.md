# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/), and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- **[api]** Widgets now cache results — previously every call hit the network and timed out under load. Activate with `cache: true` in config.
- **[cli]** Add `--json` flag for machine-readable output from `widget list`.

### Fixed

- Fix `normalize_path` to strip trailing slashes before building cache keys (#41).
- **[docs]** Refactored WidgetCache internal map type; no user-visible change.

## [1.3.0] - 2026-06-01

This release adds widget templates and fixes a crash on empty config.

### Added

- Widget templates can be loaded from a directory ([#30](https://keepachangelog.com/en/1.1.0/)).

### Fixed

- Fixed a crash on empty config ([#28](https://keepachangelog.com/en/1.1.0/)).

[Unreleased]: https://keepachangelog.com/en/1.1.0/
[1.3.0]: https://keepachangelog.com/en/1.1.0/
