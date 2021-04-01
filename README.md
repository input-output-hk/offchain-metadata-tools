# Off-chain metadata tools project

## Components

This project consists of a number of components:
  - token-metadata-creator
    - Asset/Token metadata creation and validation
  - metadata-server
    - Server for retrieving metadata from the storage layer
  - metadata-store-postgres
    - Postgres metadata storage layer
  - metadata-webhook
    - GitHub webhook that writes metadata to the storage layer
  - metadata-validator-github
    - Simple validator used to ensure that GitHub PRs are in a format appropriate for the webhook
  - metadata-lib
    - shared library for common metadata needs

For detailed documentation on the metadata ecosystem, please see [the docs](./docs/main.org).

## Development

```
# Launch a ghcid session for the given target
make dev target=lib:metadata-lib
make dev target=exe:metadata-server
make dev target=exe:metadata-webhook
# Launch a ghci session for the given target
make repl target=lib:metadata-lib
```
