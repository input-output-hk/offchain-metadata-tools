

# Acceptance tests
The document describes few high-level end-to-end scenarios that can be used as acceptance tests.

## Scenarios
1. Add new metadata
2. Update metadata
3. Test metadata-sync script

### Add new metadata

0. Mint a new token on testnet, like https://github.com/input-output-hk/cardano-node/blob/master/scripts/byron-to-mary/mint.sh

1. Add metadata entry to testnet metadata registry:

```
$ git clone https://github.com/input-output-hk/metadata-registry-testnet.git
$ cd metadata-registry-testnet/registry
```

```
$ token-metadata-creator entry --init bb59e0d0065c3368e0b7add747f795026a93489b550bf0ddfbd6dc636f6666636861696e546f6f6c7354657374696e67

$ token-metadata-creator entry bb59e0d0065c3368e0b7add747f795026a93489b550bf0ddfbd6dc636f6666636861696e546f6f6c7354657374696e67 \
  --name "OffchainToolsCoin" \
  --description "Test OffchainTools" \
  --policy /home/piotr/t/node/tokens/minter/offchain-tools-test/policy.script

$ token-metadata-creator entry bb59e0d0065c3368e0b7add747f795026a93489b550bf0ddfbd6dc636f6666636861696e546f6f6c7354657374696e67 \
  --ticker "OTC" \
  --url "https://github.com/input-output-hk/offchain-metadata-tools/" \
  --logo "/home/piotr/wb/offchain-metadata-tools/token-metadata-creator/test/testData/icon.png" \
  --decimals 5

$ token-metadata-creator entry bb59e0d0065c3368e0b7add747f795026a93489b550bf0ddfbd6dc636f6666636861696e546f6f6c7354657374696e67 -a /home/piotr/t/node/tokens/minter/offchain-tools-test/policy.skey

$ token-metadata-creator entry bb59e0d0065c3368e0b7add747f795026a93489b550bf0ddfbd6dc636f6666636861696e546f6f6c7354657374696e67 --finalize

$ token-metadata-creator validate bb59e0d0065c3368e0b7add747f795026a93489b550bf0ddfbd6dc636f6666636861696e546f6f6c7354657374696e67.json

[Info]    [Main.log#281] Wallet metadata validation successful!
```

In the case of Plutus smart-contracts follows the same process but omits the `--policy` field:

```
$ token-metadata-creator entry --init bb59e0d0065c3368e0b7add747f795026a93489b550bf0ddfbd6dc636f6666636861696e546f6f6c7354657374696e67

$ token-metadata-creator entry bb59e0d0065c3368e0b7add747f795026a93489b550bf0ddfbd6dc636f6666636861696e546f6f6c7354657374696e67 \
  --name "OffchainToolsCoin" \
  --description "Test OffchainTools"

$ token-metadata-creator entry bb59e0d0065c3368e0b7add747f795026a93489b550bf0ddfbd6dc636f6666636861696e546f6f6c7354657374696e67 \
  --ticker "OTC" \
  --url "https://github.com/input-output-hk/offchain-metadata-tools/" \
  --logo "/home/piotr/wb/offchain-metadata-tools/token-metadata-creator/test/testData/icon.png" \
  --decimals 5

$ token-metadata-creator entry bb59e0d0065c3368e0b7add747f795026a93489b550bf0ddfbd6dc636f6666636861696e546f6f6c7354657374696e67 -a /home/piotr/t/node/tokens/minter/offchain-tools-test/policy.skey

$ token-metadata-creator entry bb59e0d0065c3368e0b7add747f795026a93489b550bf0ddfbd6dc636f6666636861696e546f6f6c7354657374696e67 --finalize

$ token-metadata-creator validate bb59e0d0065c3368e0b7add747f795026a93489b550bf0ddfbd6dc636f6666636861696e546f6f6c7354657374696e67.json

[Info]    [Main.log#293] Wallet metadata validation successful!
```

2. Commit file and create PR on https://github.com/input-output-hk/metadata-registry-testnet.git

3. Validate PR.
```
$ metadata-validator-github --no-auth input-output-hk metadata-registry-testnet 94 --expect-branch master

[Info]    [Cardano.Metadata.Validation.GitHub.gitHubValidationRules#146] Validating 1 files.
[Info]    [Cardano.Metadata.Validation.GitHub.validatePRFile#198] Adding a record...

```
4. Merge PR.

5. Check that metadata is displayed in cardano-wallet which holds tokens for which we defined metadata in metadata-registry:

 - cardano-wallet must be started with `--token-metadata-server https://metadata.cardano-testnet.iohkdev.io/` parameter
```
cardano-wallet serve
  --node-socket ../node/node.socket \
  --testnet testnet-byron-genesis.json  \
  --token-metadata-server https://metadata.cardano-testnet.iohkdev.io/ \
  --database ./wallet-db
```

```
curl -X GET http://localhost:8090/v2/wallets/1b0aa24994b4181e79116c131510f2abf6cdaa4f/assets | jq
...
  {
    "asset_name": "6f6666636861696e546f6f6c7354657374696e67",
    "fingerprint": "asset19ctrcwqhl2ttx97qr4n590v382k8l59getman3",
    "policy_id": "bb59e0d0065c3368e0b7add747f795026a93489b550bf0ddfbd6dc63",
    "metadata": {
      "url": "https://github.com/input-output-hk/offchain-metadata-tools/",
      "name": "OffchainToolsCoin",
      "ticker": "OTC",
      "logo": "...",
      "description": "Test OffchainTools",
      "decimals": 5
    }
  },
```

## Update metadata

1. Update name in the already existing metadata:
```
$ token-metadata-creator entry bb59e0d0065c3368e0b7add747f795026a93489b550bf0ddfbd6dc636f6666636861696e546f6f6c7354657374696e67 \
  --name "OffchainToolsCoin update"

$ token-metadata-creator entry bb59e0d0065c3368e0b7add747f795026a93489b550bf0ddfbd6dc636f6666636861696e546f6f6c7354657374696e67 -a /home/piotr/t/node/tokens/minter/offchain-tools-test/policy.skey \
 --attest-name

$ token-metadata-creator entry bb59e0d0065c3368e0b7add747f795026a93489b550bf0ddfbd6dc636f6666636861696e546f6f6c7354657374696e67 --finalize
```

2. Verify that name `value` and `signature` are updated, also `sequenceNumber` is increased by one.

```
    "name": {
        "sequenceNumber": 1,
        "value": "OffchainToolsCoin update",
        "signatures": [
            {
                "signature": "18967d36105d3cc749a831282e52456008483fe673c86f11e423c0e56e319c3649aae9eec0a3fac8dcdf99f1e3fbd126cb8d62db49ff9c4fb88eda9bb1c4f800",
                "publicKey": "197b6c5c36b69919f3ac9952f9014de3fd096f6c7c0b34d55561942bd37d76a2"
            }
        ]
    }
```

3. Validate the update:

```
$ token-metadata-creator validate bb59e0d0065c3368e0b7add747f795026a93489b550bf0ddfbd6dc636f6666636861696e546f6f6c7354657374696e67.json

[Info]    [Main.log#281] Wallet metadata validation successful!
```

4. Commit file and create PR on https://github.com/input-output-hk/metadata-registry-testnet.git

5. Validate PR.
```
$ metadata-validator-github --no-auth input-output-hk metadata-registry-testnet 95 --expect-branch master

[Info]    [Cardano.Metadata.Validation.GitHub.gitHubValidationRules#146] Validating 1 files.
[Info]    [Cardano.Metadata.Validation.GitHub.validatePRFile#197] Modifying a record...
```
6. Merge PR.
7. Make sure that updated metadata is displayed in cardano-wallet which holds tokens for which we defined metadata in metadata-registry.
```
curl -X GET http://localhost:8090/v2/wallets/1b0aa24994b4181e79116c131510f2abf6cdaa4f/assets | jq
...
  {
    "asset_name": "6f6666636861696e546f6f6c7354657374696e67",
    "fingerprint": "asset19ctrcwqhl2ttx97qr4n590v382k8l59getman3",
    "policy_id": "bb59e0d0065c3368e0b7add747f795026a93489b550bf0ddfbd6dc63",
    "metadata": {
      "url": "https://github.com/input-output-hk/offchain-metadata-tools/",
      "name": "OffchainToolsCoin update",
      "ticker": "OTC",
      "logo": "...",
      "description": "Test OffchainTools",
      "decimals": 5
    }
  },

```

## Metadata sync

- A sync script is setup to run hourly. This script pulls the current GitHub repository state into the Postgres database.

- To test that this script is working correctly, we need to:

  1. Disable the metadata-registry-testnet webhook
    - Navigate to https://github.com/input-output-hk/metadata-registry-testnet/settings/hooks/279167325
    - Set the webhook to "Inactive"
    - Update the webhook

  2. Modify, in some way, the metadata entry created in the previous tests.

  3. Commit and push this modification directly to the master branch of `input-output-hk/metadata-registry-testnet` (or go through a PR, either will work).

  4. Confirm that querying https://metadata.cardano-testnet.iohkdev.io/metadata/${SUBJECT} returns your /original/ entry.

  5. Set the webhook to "Active" again.

  6. Wait until a few seconds past the top of the hour and ensure that querying https://metadata.cardano-testnet.iohkdev.io/metadata/${SUBJECT} returns your /modified/ entry.
