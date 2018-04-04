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
type FormM v i r = State (FormConfig v i r)

-- | Runs a form builder, retrieving the built value
runFormBuilder :: ∀ v i r a. FormM v i r a -> a
runFormBuilder = (flip evalState) { supply: 0, inputs: Map.empty }

-- A configuration supplying an incrementing supply of identifiers and a graph
-- of fields.
type FormConfig v i r =
  { supply :: Int
  , inputs :: Map InputRef (InputConfig v i r)
  }

-- | Our eventual built form
-- b: phantom type representing what it ought to parse to
type Form v i r b =
  { fields :: Map InputRef (InputConfig v i r) }

-- | A unique identifier for fields in a form
newtype InputRef = InputRef Int
derive instance newtypeInputRef :: Newtype InputRef _
derive instance eqInputRef :: Eq InputRef
derive instance ordInputRef :: Ord InputRef
derive instance genericInputRef :: Generic InputRef _
instance showInputRef :: Show InputRef where
  show = genericShow

-- v: the possible validations you'd like to
--    run in this form (as constructors)
-- i: the possible input types for the form
-- r: the possible set of relations for the form
type InputConfig v i r =
  { inputType   :: i
  , relations   :: Array r
  , validations :: Array v
  --  , triggers    :: Array t
  }

-- | Insert a new input into the form. Returns the created ref.
input :: ∀ v i r. i -> FormM v i r InputRef
input inputType = do
  st <- get
  let ref = InputRef st.supply
  modify _
    { supply = st.supply + 1
    , inputs = Map.insert ref { inputType, relations: [], validations: [] } st.inputs }
  pure ref

-- | Augment an input with new validation. Returns the original ref.
validate :: ∀ v i r. v -> InputRef -> FormM v i r InputRef
validate validation ref = do
  st <- get
  let f = \v -> Just $ v { validations = validation : v.validations }
  modify _ { inputs = Map.update f ref st.inputs }
  pure ref

-- | Augment an input with a new relationship
relate :: ∀ v i r. r -> InputRef -> FormM v i r InputRef
relate relation ref = do
  let f = \v -> Just $ v { relations = relation : v.relations }
  modify \st -> st { inputs = Map.update f ref st.inputs }
  pure ref
