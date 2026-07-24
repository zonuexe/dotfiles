# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [2.0.0] - 2026-05-15

Breaking cleanup of the auth API and a security fix for session cookies.

### Changed

- Auth tokens are now bearer-only; query-string tokens are rejected ([#80](https://github.com/acme/authkit/pull/80)).

### Security

- Session cookies are marked HttpOnly and Secure by default ([#79](https://github.com/acme/authkit/pull/79)).

[Unreleased]: https://github.com/acme/authkit/compare/v2.0.0...HEAD
[2.0.0]: https://github.com/acme/authkit/compare/v1.9.0...v2.0.0
