import Test.Tasty
    ( defaultMain, testGroup )

import qualified Test.Cardano.Metadata.Validation.Wallet

main :: IO ()
main = do
  defaultMain $
    testGroup "token-metadata-creator tests"
      [ Test.Cardano.Metadata.Validation.Wallet.tests
      ]
