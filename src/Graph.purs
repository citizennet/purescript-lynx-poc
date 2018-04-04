module Lynx.Graph where

import Prelude

import Control.Monad.State (State, evalState, get, modify)
import Data.Array ((:))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)

-- The State transformer over FormConfig
type FormM = State FormConfig

-- A configuration supplying an incrementing supply of identifiers and a graph
-- of fields
type FormConfig =
  { supply :: Int
  , inputs :: Map InputRef InputConfig
  }

-- | Our eventual built form
type Form = { fields :: Map InputRef InputConfig }

-- A unique identifier for fields in a form
newtype InputRef = InputRef Int
derive instance newtypeInputRef :: Newtype InputRef _
derive instance eqInputRef :: Eq InputRef
derive instance ordInputRef :: Ord InputRef
derive instance genericInputRef :: Generic InputRef _
instance showInputRef :: Show InputRef where
  show = genericShow

type InputConfig =
  { "type" :: FormInput
  , name :: String
  , edges :: Array Edge
  }

-- | Define possible input types
data FormInput
  = Text
  | Password
  | Email
  | TypeaheadInput
derive instance genericFormInput :: Generic FormInput _
instance showFormInput :: Show FormInput where
  show = genericShow

-- | Define relationships among fields
data Edge
  = MustEqual InputRef
  | Clear InputRef
derive instance genericEdge :: Generic Edge _
instance showEdge :: Show Edge where
  show = genericShow

-- | Insert a new input into the form
input :: FormInput -> String -> FormM InputRef
input inputType name  = do
  st <- get
  let ref = InputRef st.supply
  modify _
    { supply = st.supply + 1
    , inputs = Map.insert ref { "type": inputType, name, edges: [] } st.inputs }
  pure ref

-- | Assert that two fields must equal one another. Returns the original
-- ref for successive binding.
mustEqual :: InputRef -> InputRef -> FormM InputRef
mustEqual a b = do
  st <- get
  let f = \v -> Just $ v { edges = MustEqual b : v.edges }
  modify _ { inputs = Map.update f a st.inputs }
  pure a

-- | Assert that another field should be cleared on change
clear :: InputRef -> InputRef -> FormM InputRef
clear a b = do
  st <- get
  let f = \v -> Just $ v { edges = Clear b : v.edges }
  modify _ { inputs = Map.update f a st.inputs }
  pure a

-- | Runs a form builder, retrieving the built value
runFormBuilder :: ∀ a. FormM a -> a
runFormBuilder = (flip evalState) { supply: 0, inputs: Map.empty }
