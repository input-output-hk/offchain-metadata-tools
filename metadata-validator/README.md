# Metadata validator

A tool that validates PRs against the metadata-registry.

It is run as part of the CI process, to only allow submissions to the
metadata registry that are well-formed.

## Running

The tool can be run locally with:

```
$ metadata-validator input-output-hk metadata-registry-testnet 4 --no-auth --debug
$ metadata-validator --help
```

## Building

```
(in root of metadata-server project)
$ nix-build -A metadata-validator
```
