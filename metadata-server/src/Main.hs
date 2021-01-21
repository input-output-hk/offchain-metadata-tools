{-# OPTIONS_HADDOCK show-extensions #-}

{- |
  Module – Module name
  Description – Short description
  Maintainer – samuel.evans-powell@iohk.io
  Stability – experimental
-}

module Main where

import Data.Text (Text)

type MetadataServer =
  "metadata" :>
    ( Capture "subject" Subject                                                    :> Get '[JSON] Entry
 :<|> Capture "subject" Subject :> "properties"                                    :> Get '[JSON] Entry
 :<|> Capture "subject" Subject :> "properties" :> Capture "properties" [Property] :> Get '[JSON] AnyProperty
 :<|> "query" :> ReqBody '[JSON] BatchRequest :> Post '[JSON] BatchResponse
    )

-- TODO Take "write" and "read" functions as arguments to a function, expose that function
-- Allows users of this library to easily choose their DB backend
-- We'll use psql database backend here.

-- TODO Ensure file size < 400kb

main :: IO ()
main = putStrLn "Hello metadata-server"

-- | More loosely-typed metadata property useful in looser-typed
-- contexts.
-- 
-- For example, when the user queries the metadata server for a
-- property, we might return a preimage, or a subject, or an owner, or
-- some other generic property.
type AnyProperty = PropertyPreImage PreImage
                 | PropertySubject  Subject
                 | PropertyOwner    Owner
                 | PropertyGeneric  Property

-- | More loosely-typed metadata entry useful in looser-typed contexts.
--
-- For example, when the user queries with a batch request, they may
-- ask for the "description" and "owner" properties. We can't return
-- them a 'Entry' because we only have the "description" and
-- "owner" properties, therefore, we need a type which allows us to
-- represent part of a 'Entry', hence 'PartialEntry'.
data PartialEntry
  = PartialEntry { peSubject    :: Subject
                 , peProperties :: [AnyProperty]
                 }

-- | Represents the content of a batch request to the metadata system.
--
-- For example, @BatchRequest ["a", "b"] ["preimage", "name"]@ would
-- represent a request for the "preimage" and "name" properties of both
-- subject "a" and "b".
data BatchRequest
  = BatchRequest { bReqSubjects      :: [Subject]
                 , bReqPropertyNames :: [Text]
                 }

-- | Represents the response of a batch request.
data BatchResponse
  = BatchResponse { bRespSubjects :: [PartialEntry] }

-- | An entry in the metadata system.
data Entry
  = Entry { enSubject     :: Subject
          -- ^ The metadata subject, the on-chain identifier
          , enOwner       :: Owner
          -- ^ Public key and signature attesting to ownership of the metadata entry in this registry.
          , enName        :: Name
          -- ^ A human-readable name for the metadata subject, suitable for use in an interface
          , enDescription :: Description
          -- ^ A human-readable description for the metadata subject, suitable for use in an interface
          , enPreImage    :: PreImage
          -- ^ A pair of a hash function identifier and a bytestring, such that the bytestring is the preimage of the metadata subject under that hash function
          }

-- | The metadata subject, the on-chain identifier
type Subject = Text

-- | Public key and signature attesting to ownership of the metadata
-- entry in this registry.
data Owner
  = Owner { ownPublicKey :: Text
          , ownSignature :: Text
          }

-- | A human-readable name for the metadata subject, suitable for use in an interface
type Name = Property

-- | A human-readable description for the metadata subject, suitable for use in an interface
type Description = Property

-- | A pair of a hash function identifier and a bytestring, such that
-- the bytestring is the preimage of the metadata subject under that hash
-- function.
data PreImage
  = PreImage { piValue  :: Text
             , piHashFn :: HashFn
             }

-- | A pair of the value, and a list of annotated signatures.
data Property
  = Property { propValue        :: Text
             , propAnSignatures :: [AnnotatedSignature]
             }

-- | A pair of a public key, and a signature of the metadata entry by
-- that public key.
data AnnotatedSgnature =
  AnnotatedSgnature { asPublicKey :: Text
                    , asSignature :: Text
                    }

-- | Hash functions supported by 'PreImage'.
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
