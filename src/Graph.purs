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

----------
-- Form

-- The State transformer over FormConfig, allows us to use a monadic DSL based
-- on State to successively transform a configuration record.
--
-- v: Validations supported in this particular form
type FormM v = State (FormConfig v)

-- | Runs a form builder, retrieving the built value
runFormBuilder :: ∀ v a. FormM v a -> a
runFormBuilder = (flip evalState) { supply: 0, inputs: Map.empty }

-- A configuration supplying an incrementing supply of identifiers and a graph
-- of fields.
type FormConfig v =
  { supply :: Int
  , inputs :: Map InputRef (InputConfig v)
  }

-- | Our eventual built form
type Form v =
  { fields :: Map InputRef (InputConfig v) }

-- | A unique identifier for fields in a form
newtype InputRef = InputRef Int
derive instance newtypeInputRef :: Newtype InputRef _
derive instance eqInputRef :: Eq InputRef
derive instance ordInputRef :: Ord InputRef
derive instance genericInputRef :: Generic InputRef _
instance showInputRef :: Show InputRef where
  show = genericShow

-- v represents the possible validations you'd like to
-- run in this form (as constructors)
type InputConfig v =
  { "type"      :: FormInput
  , name        :: String
  , edges       :: Array Edge
  , validations :: Array v
  --  , tasks :: Array Task
  }

-- | Define possible input types
data FormInput
  = Text
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
input :: ∀ v. FormInput -> String -> FormM v InputRef
input inputType name  = do
  st <- get
  let ref = InputRef st.supply
  modify _
    { supply = st.supply + 1
    , inputs = Map.insert ref { "type": inputType, name, edges: [], validations: [] } st.inputs }
  pure ref

validate :: ∀ v. v -> InputRef -> FormM v InputRef
validate validation ref = do
  st <- get
  let f = \v -> Just $ v { validations = validation : v.validations }
  modify _ { inputs = Map.update f ref st.inputs }
  pure ref

-- | Assert that two fields must equal one another. Returns the original
-- ref for successive binding.
mustEqual :: ∀ v. InputRef -> InputRef -> FormM v InputRef
mustEqual b a = do
  st <- get
  let fa = \v -> Just $ v { edges = MustEqual b : v.edges }
      fb = \v -> Just $ v { edges = MustEqual a : v.edges }
  modify _ { inputs = Map.update fb b $ Map.update fa a st.inputs }
  pure a

-- | Assert that another field should be cleared on change
clear :: ∀ v. InputRef -> InputRef -> FormM v InputRef
clear b a = do
  st <- get
  let f = \v -> Just $ v { edges = Clear b : v.edges }
  modify _ { inputs = Map.update f a st.inputs }
  pure a
