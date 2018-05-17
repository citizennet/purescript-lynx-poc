module Test.Main where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Argonaut (jsonSingletonObject)
import Data.Argonaut.Core (Json, fromArray, fromNumber, fromObject, fromString, jsonNull)
import Data.Either (Either(..))
import Data.StrMap as StrMap
import Data.Tuple (Tuple(..))
import Lynx.Data.ForeignAPI (Search, fetch, findItems, unpackItems)
import Network.HTTP.Affjax (AJAX)
import Network.RemoteData (RemoteData(..))
import Test.Unit (suite, test)
import Test.Unit.Assert (equal, expectFailure)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

type IO =
  ( console :: CONSOLE
  , testOutput :: TESTOUTPUT
  , avar :: AVAR
  , ajax :: AJAX
  )

main :: Eff IO Unit
main = runTest do
   suite "JSON Parsing" do
     let sampleUrl = "https://swapi.co/api/people/?search="
         sampleSearch = "Luke"

         sampleAKey :: Array (Either Int String)
         sampleAKey = [ Right "results" ]

         sampleIKey :: Array (Either Int String)
         sampleIKey = [ Right "name" ]

         -- Looks like the URL we need to provide to the typeahead!
         sampleFetch :: Search -> Aff IO (RemoteData String (Array String))
         sampleFetch = fetch sampleAKey sampleIKey sampleUrl

         -- Example response from the SW API
         sampleJson :: Json
         sampleJson = fromObject $ StrMap.fromFoldable
           [ Tuple "count" (fromNumber 1.0)
           , Tuple "next" jsonNull
           , Tuple "previous" jsonNull
           , Tuple "results" $ fromArray
               [ jsonSingletonObject "name" $ fromString "Luke Skywalker" ]
           ]

     test "Successfully parses sample data" do
       let result = unpackItems sampleIKey =<< findItems sampleAKey sampleJson
       equal (Right [ "Luke Skywalker" ]) result

     test "Successfully parses live data" do
       result <- sampleFetch "Anakin"
       equal (Success [ "Anakin Skywalker" ]) result

     test "Fails if array keys are wrong" do
       let result = unpackItems sampleIKey =<< findItems [] sampleJson
       expectFailure
         "Fails if keys to find item array are wrong"
          $ equal (Right [ "Anakin Skywalker" ]) result

     test "Fails if item keys are wrong" do
       let result = unpackItems [] =<< findItems sampleAKey sampleJson
       expectFailure
         "Fails if keys to unpack items are wrong"
          $ equal (Right [ "Anakin Skywalker" ]) result

----------
-- JSON Parsing Tests

