# ignore-flakes

![activity flow](./readme-assets/ignore-flakes-flow.svg)

A command envelope that tracks a repeated command's successes, and ignores
command failures if the command has succeeded recently.

It is useful for commands that are flaky and are required to succeed only
ocassionally.

## Installation

Build and add `ignore-flakes` executable to `$HOME/.local/bin`:

```bash
stack install
```
