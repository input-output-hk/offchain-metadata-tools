import           Test.Tasty (TestTree, defaultMain, testGroup)

import qualified Test.Cardano.Metadata


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = do
  testGroup "Unit tests"
    [ Test.Cardano.Metadata.tests
    ]
