# Release Checklist

Use this document when making a release.

## Preparing the release

- [ ] Deploy offchain-metadata-tools to testnet such that metadata-server is available at https://metadata.cardano-testnet.iohkdev.io/ and new metadata can be added in https://github.com/input-output-hk/metadata-registry-testnet

## Create the release notes

- [ ] Add a draft release to https://github.com/input-output-hk/offchain-metadata-tools/releases, so that the release notes can be reviewed.

## Acceptance tests

_It is assumed that offchain-metadata-tools have been deployed to testnet infrastructure prior to the release so e2e acceptance tests can be executed._

- [ ] Verify the release notes point to relevant API documentation and manual.

- [ ] Download release artifacts from the release page and make them available on `$PATH`.

- [ ] Go over e2e acceptance tests -> https://github.com/input-output-hk/offchain-metadata-tools/tree/master/test/Acceptance.md

- [ ] Run locally automated scenarios from https://github.com/input-output-hk/offchain-metadata-tools/tree/master/token-metadata-creator/test
```
$ npm install
$ npm test
```

## Publication

- [ ] Once everyone has signed off (i.e. Tech lead, QA, Ops & Release manager), publish the release.
