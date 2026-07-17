# metadata-webhook

A GitHub webhook receiver. On a `push` event to a configured GitHub
repository, it fetches the added/modified metadata files from that
repository via the GitHub API and writes them to the Postgres-backed
metadata storage layer (deleting entries for removed files). It is the
write side of the off-chain metadata system; [metadata-server](../metadata-server)
is the read side.

## Command line

```
metadata-webhook --github-owner OWNER --github-repo REPO --db DB_NAME --db-user DB_USER
                  [--db-pass DB_PASS] [--db-host DB_HOST] [--db-port DB_PORT]
                  [--db-table DB_TABLE] [--db-conns INT] [-p|--port PORT]
```

| Option              | Required | Default            | Description                                                                                     |
| ---                 | ---      | ---                 | ---                                                                                              |
| `--github-owner`    | yes      | --                  | Owner (user or organization) of the sole GitHub repository this webhook will fetch file contents from |
| `--github-repo`     | yes      | --                  | Name of the sole GitHub repository this webhook will fetch file contents from                   |
| `--db`              | yes      | --                  | Name of the database to store and read metadata from                                            |
| `--db-user`         | yes      | --                  | User to connect to the metadata database with                                                   |
| `--db-pass`         | no       | (none)              | Password to connect to the metadata database with                                               |
| `--db-host`         | no       | `/run/postgresql`   | Host for the metadata database connection                                                       |
| `--db-port`         | no       | `5432`              | Port for the metadata database connection                                                       |
| `--db-table`        | no       | `metadata`          | Table in the database to store metadata in. Must be a plain SQL identifier (letters, digits, underscore; not digit-led) -- anything else is rejected at startup |
| `--db-conns`        | no       | `1`                 | Number of connections to open to the database                                                   |
| `-p`, `--port`      | no       | `8080`              | Port to run the metadata webhook server on                                                       |

`--github-owner`/`--github-repo` pin the single repository this webhook
will ever fetch file contents from. The GitHub API URL is built entirely
from these two values, never from the webhook payload itself; a `push`
event for any other repository is logged and ignored before any GitHub
API request is made.

## Environment variables

| Variable                  | Required | Description                                                                                       |
| ---                        | ---      | ---                                                                                                |
| `METADATA_WEBHOOK_SECRET`  | yes      | The shared secret configured on the GitHub webhook, used to verify request signatures. If this is unset or empty, the process logs an error and exits immediately -- it never starts up with an empty (i.e. publicly guessable) key |
| `METADATA_GITHUB_TOKEN`    | no       | A GitHub token attached as the `Authorization` header when fetching file contents, needed for private repositories or to avoid GitHub's public API rate limits. If unset or empty, requests are made anonymously (no `Authorization` header is sent) |

## Request signature verification

GitHub signs every webhook delivery with both a legacy SHA-1 HMAC
(`X-Hub-Signature`) and a SHA-256 HMAC (`X-Hub-Signature-256`), both
computed over the raw request body using `METADATA_WEBHOOK_SECRET`. This
service verifies both before a request is decoded and handled -- a
request must have a valid signature under each header to be accepted.

## How to build

See [the docs](https://input-output-hk.github.io/offchain-metadata-tools/#build-the-project-from-source).
