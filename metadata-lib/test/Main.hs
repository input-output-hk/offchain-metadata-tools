import           Test.Tasty (TestTree, defaultMain, testGroup)

import qualified Test.Cardano.Metadata.Server.Types


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = do
  testGroup "Unit tests"
    [ Test.Cardano.Metadata.Server.Types.tests
    ]
