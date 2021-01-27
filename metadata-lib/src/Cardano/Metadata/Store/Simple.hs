
module Cardano.Metadata.Store.Simple where

import qualified Data.Map.Strict as M
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)

import Cardano.Metadata.Server.Types
import Cardano.Metadata.Store.Types 

readFns :: ReadFns
readFns =
  ReadFns
    (pure . getEntryForSubject)
    (\subj -> pure . getPartialEntryForProperty subj)
    (pure . getBatch)

getEntryForSubject :: Subject -> Either ReadError Entry
getEntryForSubject subj = case M.lookup subj dat of
  Nothing -> Left $ NoSubject subj
  Just e  -> Right e

getPartialEntryForProperty :: Subject -> Text -> Either ReadError PartialEntry
getPartialEntryForProperty subj prop = do
  entry <- getEntryForSubject subj
  getProperty subj prop entry

getProperty :: Subject -> Text -> Entry -> Either ReadError PartialEntry
getProperty subj prop entry =
  case Aeson.toJSON entry of
    (Aeson.Object obj) -> case HM.lookup prop obj of
      Nothing -> Left $ NoProperty subj prop
      Just p  -> case Aeson.fromJSON (Aeson.Object $ HM.fromList [("subject", Aeson.String subj), (prop, p)]) of
        Aeson.Error str -> error $ "JSON parsing error: " <> str
        Aeson.Success x -> Right x
    otherwise          -> error "Entry isn't a JSON Object but should be."

getBatch :: BatchRequest -> BatchResponse
getBatch (BatchRequest subjs props) =
  BatchResponse $
    flip foldMap subjs $ \subj ->
      flip foldMap props $ \prop ->
        case getPartialEntryForProperty subj prop of
          Left _err -> []
          Right x   -> [x]
