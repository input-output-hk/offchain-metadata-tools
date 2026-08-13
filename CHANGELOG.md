# Revision history for offchain-metadata-tools

## 0.5.0.0 -- 2026-08-12

### Security

* metadata-server: Fixed SQL injection in the batch query endpoint.
* metadata-store-postgres, metadata-sync: Fixed SQL injection via the metadata
  table name.
* metadata-webhook: Refuse to start when `METADATA_WEBHOOK_SECRET` is unset or
  empty.
* metadata-webhook: Pin GitHub API requests to the repository given by the new
  `--github-owner` and `--github-repo` options.
* metadata-webhook: Verify GitHub's `X-Hub-Signature-256` in addition to the
  legacy SHA-1 `X-Hub-Signature`.
* metadata-webhook: Treat an unset or empty `METADATA_GITHUB_TOKEN` as
  anonymous access.
* nixos: Removed the `webHookSecret` and `gitHubToken` options, which placed
  secrets in the world-readable Nix store. `environmentFile` is now required
  and is the only supported mechanism.
* metadata-server, metadata-sync, metadata-webhook: Obfuscate database
  passwords in the connection string log line.
* nixos: Added an optional `environmentFile` to the metadata-server and
  metadata-sync modules so a remote database password can be supplied as
  `PGPASSWORD` rather than via `--db-pass`.
* ci: Default the workflow to `contents: read`, pin actions by SHA, and build
  pull requests including those from forks.

### Breaking changes

* metadata-webhook: `--github-owner` and `--github-repo` are new and required.
* All services: `--db-table` must be a plain SQL identifier (letters, digits,
  underscore, not digit-led) and is rejected at startup otherwise.
* nixos: metadata-webhook gains required `githubOwner` and `githubRepo`
  options; `environmentFile` changes from `nullOr str` to a required `str`.
* nixos: `metadataServerPkgs` and `offchainMetadataToolsPkgs` now default to
  `.project.hsPkgs` rather than `.project`, and the services order after
  `postgresql-setup.service` instead of the non-existent `postgres.service`.
* nix: `flake.nix` is the single source of truth. Removed `release.nix`, the
  niv `nix/sources.*` files, `nix/cabal-shell.nix` and
  `scripts/check-hydra.nix`; `default.nix` and `shell.nix` are flake-compat
  shims. Dropped `x86_64-darwin`. Hydra jobs move to `hydraJobs` with a
  `required` aggregate, and `nixosModules` is exported.
* metadata-lib: `appSigned` takes the raw secret as a new first argument;
  `getFileContent` takes `Maybe GitHubToken` and a `GitHubRepo`;
  `RepositoryInfo` carries `repoInfoFullName` instead of `repoInfoContentsUrl`.
  New `Cardano.Metadata.Webhook.Signature` module.
* metadata-store-postgres: New `InvalidTableName` constructor on
  `PostgresKeyValueException`; throw sites moved from `throw` to `throwIO`.
* Toolchain: GHC 8.10.7 to 9.6.7, aeson 1 to 2, mtl 2.3, resource-pool 0.4.
  Added CHaP with updated index-states.  All source-repository-package pins
  dropped except servant-github-webhook, which needs a patch for aeson 2.
* token-metadata-creator: Reworked for the cardano-api 10.x unified
  `SimpleScript`, preserving the frozen `[tag, script]` CBOR wire format and
  time-locks-always-satisfiable policy semantics. Signing keys are read via
  cardano-api instead of cardano-cli, with a bech32 fallback.
* token-metadata-creator: Replaced the npm test harness with a Haskell golden
  test suite pinning the policy wire format and attestation verification
  against real mainnet registry entries.

### Other changes

* metadata-server, metadata-sync, metadata-webhook: Added `--db-pass` and
  `--db-port` so a remote Postgres can be used.
* metadata-server, metadata-sync, metadata-webhook: Line-buffer stdout and
  stderr for journald.
* metadata-server: Log over a channel drained by the main thread and filter
  debug output.
* metadata-webhook: Added a README covering the CLI, environment variables and
  signature verification.
* Added unit tests for webhook secret resolution and SHA-256 signature
  verification. NixOS tests updated for current nixpkgs.
* ci: Moved from BuildKite to GitHub Actions. Removed bors and tullia.
* All packages: Set `maintainer` to operations@iohk.io, appended the IOHK
  Engineering Team to `author`, and added the missing `homepage`,
  `bug-reports`, `license`, `license-file` and `copyright` fields.

## 0.4.0.0 -- 2022-01-20

* token-metadata-creator: Increased ticker max length from 5 to 9.

## 0.3.0.0 -- 2021-08-30

* token-metadata-creator: Made "policy" field optional to support Plutus smart
  contracts.

## 0.2.0.0 -- 2020-05-25

* Added support for the "decimals" field.
* Fixed desynchronization between GitHub metadata repository and the metadata server.
* token-metadata-creator: Improved error messages received when inputting data from the CLI.
* token-metadata-creator: Support the use of non-UTF8 locales in token-metadata-creator.

## 0.1.0.0 -- 2020-04-14

* First version.
