import           Test.Tasty (TestTree, defaultMain, testGroup, withResource)
import Data.Word (Word8)

import qualified Test.Cardano.Metadata.Server.Types
import qualified Test.Cardano.Metadata.Server
import Cardano.Metadata.Store.Types
import Test.Cardano.Metadata.Generators
import Cardano.Metadata.Store.Simple (simpleStore)

main :: IO ()
main = defaultMain testsWithStoreInterfaces

testsWithStoreInterfaces :: TestTree
testsWithStoreInterfaces =
  withResource
    (do
        simpleIntf1 <- simpleStore mempty
        simpleIntf2 <- simpleStore mempty
        pure (simpleIntf1, simpleIntf2)
    )
    (const $ pure ())
    tests

tests
  :: IO ( StoreInterface Word8 Word8
        , StoreInterface ComplexKey ComplexType
        )
  -> TestTree
tests intfs = 
  testGroup "All tests"
    [ testGroup "Type serialisation tests"
        [ Test.Cardano.Metadata.Server.Types.tests
        ]
    , testGroup "Data store tests"
        [ Test.Cardano.Metadata.Server.tests intfs
        ]
    ]
