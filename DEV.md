# Ignore Flakes Development

This README is intended for developers.

## Dev environment setup

This project requires [Stack] and other tools listed in `lefthook.yml`.

1. To setup Git hooks, install lefthook:

   ```shell
   lefthook install
   ```

## Building

To build the binary, run

```bash
stack build
```

## Releasing

A release has a specific version (e.g., `1.0.0.0`) consists of the following
artifacts:

- A tag of the form `v1.0.0.0` on a commit with a `package.yaml` with the same
  version string.
- A GitHub release containing a macOs binary.

To make a release, use a helper script: `./dev/bin/release-new-version`.

[Stack]: https://docs.haskellstack.org/en/stable/
