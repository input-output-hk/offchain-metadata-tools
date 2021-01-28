import           Test.Tasty (TestTree, defaultMain, testGroup)

import qualified Test.Cardano.Metadata.Server.Types
import qualified Test.Cardano.Metadata.Server

main :: IO ()
main = defaultMain =<< tests

tests :: IO TestTree
tests = do
  dataStoreTests <- Test.Cardano.Metadata.Server.tests
  pure $
    testGroup "All tests"
    [ testGroup "Type serialisation tests"
        [ Test.Cardano.Metadata.Server.Types.tests
        ]
    , testGroup "Data store tests"
        [ dataStoreTests
        ]
    ]
