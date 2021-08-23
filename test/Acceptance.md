

# Acceptance tests
The document describes few high-level end-to-end scenarios that can be used as acceptance tests.

## Scenarios
1. Add new metadata (for tokens minted with Plutus minting policy)
2. Add new metadata (for native tokens)
3. Update metadata
4. Test metadata-sync script

### Add new metadata (for tokens minted with Plutus minting policy)

0. Mint a new token on testnet using exemplary Plutus script, like https://github.com/input-output-hk/cardano-node/blob/master/scripts/plutus/scripts/anyone-can-mint.plutus

 - create address:
 ```
$ cardano-cli  address key-gen \
--verification-key-file payment.vkey \
--signing-key-file payment.skey

$ cardano-cli address build \
--payment-verification-key-file payment.vkey \
--out-file payment.addr \
--testnet-magic 1097911063
 ```
 - send 2 txs with ADA to this address, such that you have utxo for collateral
 ```
 $ cardano-cli query utxo --address $(cat payment.addr) --testnet-magic 1097911063
 TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
76c3338240434a6990b8d6258eea0fe0b85c3d2ec9e8e541cd1a7cec4e76643b     0        1000000000000 lovelace + TxOutDatumHashNone
fe5c852ad7aba5f2ea2f4b9078a41626f67768e09180039c872e423468116409     0        1000000000000 lovelace + TxOutDatumHashNone
 ```
 - mint token sending it to your wallet address:
 ```
 $ export MAGIC=1097911063
 $ git clone https://github.com/input-output-hk/cardano-node.git
 $ cd cardano-node/scripts/plutus
 $ cardano-cli query protocol-parameters --testnet-magic $MAGIC --out-file pparams.json

 $ cardano-cli transaction build  \
    --alonzo-era  \
    --testnet-magic $MAGIC \
    --change-address "addr_test1vzfq99mvg8lfxqa88t8d3hjxhl0w83g9c832cw269u6ht8sn5xkpl" \
    --tx-in "76c3338240434a6990b8d6258eea0fe0b85c3d2ec9e8e541cd1a7cec4e76643b#0"  \
    --tx-in-collateral "fe5c852ad7aba5f2ea2f4b9078a41626f67768e09180039c872e423468116409#0" \
    --mint-redeemer-file "data/42.redeemer" \
    --tx-out "addr_test1qqlgm2dh3vpv07cjfcyuu6vhaqhf8998qcx6s8ucpkly6f8l0dw5r75vk42mv3ykq8vyjeaanvpytg79xqzymqy5acmqtjmugu+1600000 + 1 $(cardano-cli transaction policyid --script-file scripts/anyone-can-mint.plutus).MillarCoin" \
    --mint "1 $(cardano-cli transaction policyid --script-file scripts/anyone-can-mint.plutus).MillarCoin"  \
    --mint-script-file "scripts/anyone-can-mint.plutus" \
    --protocol-params-file pparams.json  \
    --out-file plutusmint.body

 $ cardano-cli transaction sign \
   --tx-body-file plutusmint.body \
   --testnet-magic $MAGIC \
   --signing-key-file payment.skey \
   --out-file plutusmint.tx

 $ cardano-cli transaction submit --tx-file plutusmint.tx --testnet-magic $MAGIC
 ```

 1. Add metadata entry to testnet metadata registry:

 ```
 $ git clone https://github.com/input-output-hk/metadata-registry-testnet.git
 $ cd metadata-registry-testnet/registry
 ```

 ```
 $ token-metadata-creator entry --init fda1b6b487bee2e7f64ecf24d24b1224342484c0195ee1b7b943db504d696c6c6172436f696e

 $ token-metadata-creator entry fda1b6b487bee2e7f64ecf24d24b1224342484c0195ee1b7b943db504d696c6c6172436f696e \
   --name "PlutusCoin" \
   --description "Test Plutus token" \

 $ token-metadata-creator entry fda1b6b487bee2e7f64ecf24d24b1224342484c0195ee1b7b943db504d696c6c6172436f696e \
   --ticker "PLT" \
   --url "https://github.com/input-output-hk/offchain-metadata-tools/" \
   --logo "/home/piotr/wb/offchain-metadata-tools/token-metadata-creator/test/testData/icon.png" \
   --decimals 5 \


 $ token-metadata-creator entry fda1b6b487bee2e7f64ecf24d24b1224342484c0195ee1b7b943db504d696c6c6172436f696e --finalize

 $ token-metadata-creator validate fda1b6b487bee2e7f64ecf24d24b1224342484c0195ee1b7b943db504d696c6c6172436f696e.json

 [Info]    [Main.log#281] Wallet metadata validation successful!
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
     "asset_name": "4d696c6c6172436f696e",
     "fingerprint": "asset17yzqcz2jlaq3aklshfh939ccz9pgxz04md8jl9",
     "policy_id": "fda1b6b487bee2e7f64ecf24d24b1224342484c0195ee1b7b943db50",
     "metadata": {
       "url": "https://github.com/input-output-hk/offchain-metadata-tools/",
       "name": "PlutusCoin",
       "ticker": "PLT",
       "logo": "...",
       "description": "Test Plutus token",
       "decimals": 5
     }
   },
 ```

### Add new metadata (for native tokens)

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
  --decimals 5 \

$ token-metadata-creator entry bb59e0d0065c3368e0b7add747f795026a93489b550bf0ddfbd6dc636f6666636861696e546f6f6c7354657374696e67 -a /home/piotr/t/node/tokens/minter/offchain-tools-test/policy.skey

$ token-metadata-creator entry bb59e0d0065c3368e0b7add747f795026a93489b550bf0ddfbd6dc636f6666636861696e546f6f6c7354657374696e67 --finalize

$ token-metadata-creator validate bb59e0d0065c3368e0b7add747f795026a93489b550bf0ddfbd6dc636f6666636861696e546f6f6c7354657374696e67.json

[Info]    [Main.log#281] Wallet metadata validation successful!
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
