# ignore-flakes

UNDER DEVELOPMENT

![activity flow](./readme-assets/ignore-flakes-flow.svg)

A command envelope that tracks a repeated command's successes, and ignores
command failures if the command has succeed recently.

It is useful for commands that are flaky, and are required to succeed only
ocassionally.

## Development

This section is intended for developers. It describes development related matters.

### Dev environment setup

1. To install some development tools (commitlint), set up npm:

   ```shell
   npm install
   ```

2. To setup Git hooks, install lefthook:

   ```shell
   lefthook install
   ```
