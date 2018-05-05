module App.Data.Relate.Type where

import Prelude

import Data.Argonaut (jsonEmptyObject)
import Data.Argonaut.Decode (decodeJson, (.?))
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Encode ((:=), (~>))
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Either (Either(..))
import Lynx.Data.Graph (InputRef)

data Relate
  = Equals InputRef
  | Clears InputRef

instance encodeJsonRelate :: EncodeJson Relate where
  encodeJson =
    let update str i = do
          "relationType" := str
          ~> "ref" := i
          ~> jsonEmptyObject
     in case _ of
      Equals i -> update "MustEqual" i
      Clears i -> update "Clear" i

instance decodeJsonRelate :: DecodeJson Relate where
  decodeJson json = do
    obj <- decodeJson json
    ref <- obj .? "ref"
    str <- obj .? "relationType"
    relation <- case str of
      "Equals" -> pure $ Equals ref
      "Clears" -> pure $ Clears ref
      otherwise ->
        Left $ "No case written to decode:\n" <> "  " <> str <> "\n"
    pure relation
