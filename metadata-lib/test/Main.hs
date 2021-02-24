import           Test.Tasty                         (defaultMain,
                                                     testGroup)

import qualified Test.Cardano.Metadata.Server
import qualified Test.Cardano.Metadata.Server.Types
import qualified Test.Cardano.Metadata.Store.Simple
import qualified Test.Cardano.Metadata.Types
import qualified Test.Cardano.Metadata.Types.Wallet
import qualified Test.Cardano.Metadata.Attestation

main :: IO ()
main = do
  serverSpec <- Test.Cardano.Metadata.Server.tests
  defaultMain $
    testGroup "Metadata server library tests"
      [ Test.Cardano.Metadata.Types.tests
      , Test.Cardano.Metadata.Types.Wallet.tests
      , Test.Cardano.Metadata.Server.Types.tests
      , Test.Cardano.Metadata.Store.Simple.tests
      , Test.Cardano.Metadata.Attestation.tests
      , serverSpec
      ]
