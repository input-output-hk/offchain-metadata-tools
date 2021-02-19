import           Test.Tasty (TestTree, defaultMain, testGroup, withResource)
import Data.Word (Word8)

import qualified Test.Cardano.Metadata.Server.Types
import qualified Test.Cardano.Metadata.Server
import qualified Test.Cardano.Metadata.Store.Simple
import qualified Test.Cardano.Metadata.Types
import qualified Test.Cardano.Metadata.Types.Wallet
import Cardano.Metadata.Store.Types
import Test.Cardano.Metadata.Generators
import Cardano.Metadata.Store.Simple (simpleStore)

main :: IO ()
main = do
  serverSpec <- Test.Cardano.Metadata.Server.tests
  defaultMain $
    testGroup "Metadata server library tests"
      [ Test.Cardano.Metadata.Types.tests
      , Test.Cardano.Metadata.Types.Wallet.tests
      , Test.Cardano.Metadata.Server.Types.tests
      , Test.Cardano.Metadata.Store.Simple.tests
      , serverSpec
      ]
