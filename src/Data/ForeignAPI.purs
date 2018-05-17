module Lynx.Data.ForeignAPI where

import Prelude

import Control.Monad.Aff (Aff)
import Data.Array (uncons, (!!))
import Data.Argonaut (JArray, JObject, Json, foldJson, toArray, toString)
import Data.Either (Either(..), note)
import Data.Maybe (Maybe(..))
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
type Keys = Array (Either Int String)
type IO e = ( ajax :: AJAX | e )

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
findItems :: Keys -> Json -> Either String (Array Json)
findItems keys json =
  case uncons keys of
    Nothing -> note "JSON result is not an array" $ toArray json
    Just { head: x, tail: xs } ->
      case pickNext x json of
        Left str -> Left str
        Right res' -> findItems xs res'

-- And again (a bit redundant) to turn "items" into strings.
unpackItems :: Keys -> Array Json -> Either String (Array String)
unpackItems keys json =
  case uncons keys of
    Nothing ->
      note "Unable to convert supplied array to strings"
      $ traverse toString json
    Just { head: x, tail: xs } ->
      case traverse (pickNext x) json of
        Left str -> Left str
        Right res' -> unpackItems xs res'

-- We need some way to turn response JSON into an array of string options.
-- We could force the end user to change their API to work, but that sucks.
-- Instead, we ought to let them specify what keys will get the array of
-- results, and what keys will turn the JSON to a string
fetch :: ∀ e
  . Keys
 -> Keys
 -> URL
 -> Search
 -> Aff (IO e) (RemoteData String (Array String))
fetch akeys ikeys url search = do
  -- Fetch the data
  res <- _.response <$> get (url <> search)
  -- Attempt to retrieve the string array
  pure <<< fromEither $ unpackItems ikeys =<< findItems akeys res
