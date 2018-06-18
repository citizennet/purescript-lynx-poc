module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff)
import Data.Argonaut (jsonSingletonObject)
import Data.Argonaut.Core (Json, fromArray, fromNumber, fromObject, fromString, jsonNull)
import Data.Either (Either(..))
import Foreign.Object as Object
import Data.Tuple (Tuple(..))
import Lynx.Data.ForeignAPI (ArrayKeys(..), ItemKeys(..), Search, fetch, findItems, readArrayKeys, readItemKeys, renderArrayKeys, renderItemKeys, unpackItems)
import Network.RemoteData (RemoteData(..))
import Test.Unit (suite, test)
import Test.Unit.Assert (equal, expectFailure)
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
   suite "JSON Parsing" do
     let sampleUrl = "https://swapi.co/api/people/?search="
         sampleSearch = "Luke"

         sampleAKey :: ArrayKeys
         sampleAKey = ArrayKeys [ Right "results" ]

         sampleIKey :: ItemKeys
         sampleIKey = ItemKeys [ Right "name" ]

         -- Looks like the URL we need to provide to the typeahead!
         sampleFetch :: Search -> Aff (RemoteData String (Array String))
         sampleFetch = fetch sampleAKey sampleIKey sampleUrl

         -- Example response from the SW API
         sampleJson :: Json
         sampleJson = fromObject $ Object.fromFoldable
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
       let result = unpackItems sampleIKey =<< findItems (ArrayKeys []) sampleJson
       expectFailure
         "Fails if keys to find item array are wrong"
          $ equal (Right [ "Anakin Skywalker" ]) result

     test "Fails if item keys are wrong" do
       let result = unpackItems (ItemKeys []) =<< findItems sampleAKey sampleJson
       expectFailure
         "Fails if keys to unpack items are wrong"
          $ equal (Right [ "Anakin Skywalker" ]) result

     test "Renders to string properly" do
       let result = renderItemKeys sampleIKey
       equal "name" result

     test "Renders to string properly" do
       let result = renderArrayKeys (ArrayKeys [ Left 1, Right "key", Left 2 ])
       equal "1, key, 2" result

     test "Renders to string properly" do
       let result = renderArrayKeys (ArrayKeys [ ])
       equal "" result

     test "Reads from string properly" do
       let result = readItemKeys "name"
       equal sampleIKey result

     test "Reads from string properly" do
       let result = readArrayKeys "1, key, 2"
       equal (ArrayKeys [ Left 1, Right "key", Left 2 ]) result

     test "Reads from string properly" do
       let result = readArrayKeys ""
       equal (ArrayKeys [ ]) result
