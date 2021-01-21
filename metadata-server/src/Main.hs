module Main where

import Data.Text (Text)

type Subject = Text

type MetadataServer =
  "metadata" :>
    ( Capture "subject" Subject                                                    :> Get '[JSON] MetadataEntry
 :<|> Capture "subject" Subject :> "properties"                                    :> Get '[JSON] MetadataEntry
 :<|> Capture "subject" Subject :> "properties" :> Capture "properties" [Property] :> Get '[JSON] AnyMetadataProperty
 :<|> "query" :> ReqBody '[JSON] BatchRequest :> Post '[JSON] BatchResponse
    )

-- TODO Take "write" and "read" functions as arguments to a function, expose that function
-- Allows users of this library to easily choose their DB backend
-- We'll use psql database backend here.

-- TODO Ensure file size < 400kb

main :: IO ()
main = putStrLn "Hello metadata-server"

-- | More loosely-typed metadata property useful in looser typed contexts.
-- For exa
type AnyMetadataProperty = PropertyPreImage PreImage
                         | PropertySubject  Subject
                         | PropertyOwner    Owner
                         | PropertyGeneric  MetadataProperty

-- More loosely-typed metadata entry useful in looser typed contexts
data AnyMetadataEntry
  = AnyMetadataEntry { anyMeSubject    :: Subject
                     , anyMeProperties :: [AnyMetadataProperty]
                     }

data BatchRequest
  = BatchRequest { bReqSubjects      :: [Subject]
                 , bReqPropertyNames :: [Text]
                 }

data BatchResponse
  = BatchResponse { bRespSubjects :: [AnyMetadataEntry] }

 
data MetadataEntry
  = MetadataEntry { mePreImage    :: PreImage
                  , meSubject     :: Subject
                  , meDescription :: MetadataProperty
                  , meName        :: MetadataProperty
                  , meOwner       :: Owner
                  }

data PreImage
  = PreImage { piValue  :: Text
             , piHashFn :: HashFn
             }

data Owner
  = Owner { ownPublicKey :: Text
          , ownSignature :: Text
          }

data MetadataProperty
  = MetadataProperty { mpValue        :: Text
                     , mpAnSignatures :: [AnnotatedSignature]
                     }

data AnnotatedSgnature =
  AnnotatedSgnature { asPublicKey :: Text
                    , asSignature :: Text
                    }

data HashFn = Blake2b256
            | Blake2b224
            | SHA256

instance Show HashFn where
  show Blake2b256 = "blake2b-256"
  show Blake2b224 = "blake2b-224"
  show SHA256     = "sha256"

instance Read HashFn where
  readPrec = Read.lift $ choice [ pure Blake2b256 <$ string "blake2b-256"
                                , pure Black2b224 <$ string "blake2b-224"
                                , pure SHA256     <$ string "sha256"
                                ]
