# Load tests

The document describes how to run load tests against https://metadata.cardano-testnet.iohkdev.io/.
It is recommended to perform a load run before every release to gather results and make sure there is no performance deterioration or any unexpected behavior.

Performance tests are completed with locust.io, written in Python, and performed from a test engineers home computer. The users are ramped up at a rate of 10 per second until they hit the target Total Users. This is maintained for several minutes and results are then aggregated from across the entire run.

## To run the test

1. Start locust docker
```
$ cd test/load
$ make
```

2. Visit http://0.0.0.0:8089/ and run test from there.

## Pre-relese test

Total Users = 1000
Ramp up rate = 10

Execution time = 10 minutes from the point where all users ramp up.

Store results in https://github.com/input-output-hk/offchain-metadata-tools/wiki/Load-Tests-Results
