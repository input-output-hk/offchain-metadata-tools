{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Control.Monad.IO.Class
    ( liftIO )
import Data.Proxy
    ( Proxy (Proxy) )
import Data.Tagged
    ( Tagged, unTagged )
import Data.Text
    ( Text )
import Data.Word
    ( Word8 )
import Test.Tasty
    ( TestTree
    , askOption
    , defaultIngredients
    , defaultMainWithIngredients
    , includingOptions
    , testGroup
    , withResource
    )
import Test.Tasty.Options

import Cardano.Metadata.Store.File
import Cardano.Metadata.Store.Types
import Test.Cardano.Metadata.Generators
    ( ComplexType )
import Test.Cardano.Metadata.Store

newtype Folder = Folder { _folder :: FilePath }

instance IsOption Folder where
  defaultValue   = error $ "'" <> unTagged (optionName :: Tagged Folder String) <> "' is required."
  parseValue str = Just $ Folder str
  optionName     = pure "folder"
  optionHelp     = pure "Folder to store and retrieve metadata entries from."

main :: IO ()
main =
    defaultMainWithIngredients
      ((includingOptions
          [ Option (Proxy @Folder) ]
       )
       : defaultIngredients
      )
      testsWithStore

testsWithStore :: TestTree
testsWithStore =
  askOption $ \(Folder folder) ->
    withResource
      (do
        fileIntf1 <- liftIO $ fileStore folder
        fileIntf2 <- liftIO $ fileStore folder

        pure (fileIntf1, fileIntf2)
      )
      (const $ pure ())
      tests

tests
  :: IO ( StoreInterface Word8 Word8
        , StoreInterface Text ComplexType
        )
  -> TestTree
tests resources =
  testGroup "integration tests"
    [
      testGroup "File data store implementation"
       [ testKeyValueImplementation $ (\(intf, _) -> intf) <$> resources
       , testKeyValueComplexTypeImplementation $ (\(_, intf) -> intf) <$> resources
       ]
    ]
