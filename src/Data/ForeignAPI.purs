module Lynx.Data.ForeignAPI where

import Prelude

import Control.Monad.Aff (Aff)
import Data.Argonaut (JArray, JObject, Json, foldJson, toArray, toString)
import Data.Array (uncons, (!!))
import Data.Either (Either(..), note)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.StrMap as StrMap
import Data.Traversable (traverse)
import Network.HTTP.Affjax (AJAX, get)
import Network.RemoteData (RemoteData, fromEither)

-- We need to provide this function to our typeahead:
-- Search -> Aff (TA.Effects eff) (RemoteData err (Array String))

-- To do this we need to figure out what URL to hit, and how to
-- decode the response into an array of strings.
type URL = String
type Search = String

newtype ArrayKeys = ArrayKeys (Array (Either Int String))
derive instance newtypeArrayKeys :: Newtype ArrayKeys _
derive instance genericArrayKeys :: Generic ArrayKeys _
instance eqArrayKeys :: Eq ArrayKeys where
  eq = genericEq
instance showArrayKeys :: Show ArrayKeys where
  show = genericShow

newtype ItemKeys = ItemKeys (Array (Either Int String))
derive instance newtypeItemKeys :: Newtype ArrayKeys _
derive instance genericItemKeys :: Generic ItemKeys _
instance eqItemKeys :: Eq ItemKeys where
  eq = genericEq
instance showItemKeys :: Show ItemKeys where
  show = genericShow

----------
-- Code

pickKey :: String -> JObject -> Either String Json
pickKey key = note ("Failed to find key in object: " <> key) <<< StrMap.lookup key

pickIndex :: Int -> JArray -> Either String Json
pickIndex i arr = note ("Index " <> show i <> " is out of bounds!") $ arr !! i

pickNext :: Either Int String -> Json -> Either String Json
pickNext (Left i) = foldJson
  (const $ Left "Found null, expected array")
  (const $ Left "Found boolean, expected array")
  (const $ Left "Found number, expected array")
  (const $ Left "Found string, expected array")
  (\xs -> pickIndex i xs)
  (const $ Left "Found object, expected array")
pickNext (Right key) = foldJson
  (const $ Left $ "Found null, expected object with key " <> key)
  (const $ Left $ "Found boolean, expected object with key " <> key)
  (const $ Left $ "Found number, expected object with key " <> key)
  (const $ Left $ "Found string, expected object with key " <> key)
  (const $ Left $ "Found array, expected object with key " <> key)
  (\obj -> pickKey key obj)

-- With recursion we can walk through the full array with the supplied keys
-- towards the array of items
findItems :: ArrayKeys -> Json -> Either String (Array Json)
findItems (ArrayKeys keys) json =
  case uncons keys of
    Nothing -> note "JSON result is not an array" $ toArray json
    Just { head: x, tail: xs } ->
      case pickNext x json of
        Left str -> Left str
        Right res' -> findItems (ArrayKeys xs) res'

-- And again (a bit redundant) to turn "items" into strings.
unpackItems :: ItemKeys -> Array Json -> Either String (Array String)
unpackItems (ItemKeys keys) json =
  case uncons keys of
    Nothing ->
      note "Unable to convert supplied array to strings"
      $ traverse toString json
    Just { head: x, tail: xs } ->
      case traverse (pickNext x) json of
        Left str -> Left str
        Right res' -> unpackItems (ItemKeys xs) res'

-- We need some way to turn response JSON into an array of string options.
-- We could force the end user to change their API to work, but that sucks.
-- Instead, we ought to let them specify what keys will get the array of
-- results, and what keys will turn the JSON to a string
fetch :: ∀ e
  . ArrayKeys
 -> ItemKeys
 -> URL
 -> Search
 -> Aff (ajax :: AJAX | e) (RemoteData String (Array String))
fetch akeys ikeys url search = do
  -- Fetch the data
  res <- _.response <$> get (url <> search)
  -- Attempt to retrieve the string array
  pure <<< fromEither $ unpackItems ikeys =<< findItems akeys res
