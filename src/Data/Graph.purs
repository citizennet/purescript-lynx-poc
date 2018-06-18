module Lynx.Data.Graph where

import Prelude

import Control.Monad.State (State, evalState, get, modify_)
import Data.Argonaut ((.?), (~>), (:=))
import Data.Argonaut as A
import Data.Array ((:))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map (Map)
import Data.Map as Map
import Data.Newtype (class Newtype, wrap)
import Data.Traversable (traverse)

----------
-- Form

-- The State transformer over FormConfig, allows us to use a monadic DSL based
-- on State to successively transform a configuration record.
type FormM v i r = State (FormConfig v i r)

-- Runs a form builder, retrieving the built value
runFormBuilder :: ∀ v i r a. FormId -> FormM v i r a -> a
runFormBuilder i = flip evalState $ wrap { id: i, supply: 0, inputs: Map.empty }

-- A configuration supplying an incrementing supply of identifiers and a graph
-- of fields.
newtype FormConfig v i r = FormConfig
  { id :: FormId
  , supply :: Int
  , inputs :: Map InputRef (InputConfig v i r)
  }
derive instance newtypeFormConfig :: Newtype (FormConfig v i r) _
derive instance genericFormConfig :: Generic (FormConfig v i r) _

instance showFormConfig :: (Show v, Show i, Show r) => Show (FormConfig v i r) where
  show = genericShow

instance decodeJsonFormConfig
  :: (A.DecodeJson v, A.DecodeJson i, A.DecodeJson r)
  => A.DecodeJson (FormConfig v i r)
  where
  decodeJson json = do
    obj <- A.decodeJson json
    id <- obj .? "id"
    supply <- obj .? "supply"
    inputs <- obj .? "inputs"
    pure $ FormConfig { id, supply, inputs }

instance encodeJsonFormConfig :: (A.EncodeJson v, A.EncodeJson i, A.EncodeJson r) => A.EncodeJson (FormConfig v i r) where
  encodeJson (FormConfig { supply, inputs }) = do
    "supply" := supply
      ~> "inputs" := A.encodeJson inputs
      ~> A.jsonEmptyObject

-- | A unique identifier for fields in a form
newtype InputRef = InputRef Int
derive instance newtypeInputRef :: Newtype InputRef _
derive instance genericInputRef :: Generic InputRef _
derive instance eqInputRef :: Eq InputRef
derive instance ordInputRef :: Ord InputRef

instance showInputRef :: Show InputRef where
  show = genericShow

instance decodeJsonInputRef :: A.DecodeJson InputRef where
  decodeJson json = (pure <<< InputRef) =<< A.decodeJson json

instance encodeJsonInputRef :: A.EncodeJson InputRef where
  encodeJson (InputRef int) = A.encodeJson int

-- | A unique identifier for forms
newtype FormId = FormId Int
derive instance newtypeFormId :: Newtype FormId _
derive instance genericFormId :: Generic FormId _
derive instance eqFormId :: Eq FormId

instance showFormId :: Show FormId where
  show = genericShow

instance decodeJsonFormId :: A.DecodeJson FormId where
  decodeJson json = (pure <<< FormId) =<< A.decodeJson json

instance encodeJsonFormId :: A.EncodeJson FormId where
  encodeJson (FormId int) = A.encodeJson int

-- v: the possible validations you'd like to
--    run in this form (as constructors)
-- i: the possible input types for the form
-- r: the possible set of relations for the form
newtype InputConfig v i r = InputConfig
  { inputType   :: i
  , relations   :: Array r
  , validations :: Array v
  }
derive instance newtypeInputConfig :: Newtype (InputConfig v i r) _
derive instance genericInputConfig :: Generic (InputConfig v i r) _
instance showInputConfig :: (Show v, Show i, Show r) => Show (InputConfig v i r) where
  show = genericShow

instance decodeJsonInputConfig
  :: (A.DecodeJson v, A.DecodeJson i, A.DecodeJson r) => A.DecodeJson (InputConfig v i r) where
  decodeJson json = do
    obj <- A.decodeJson json
    validations <- traverse A.decodeJson =<< obj .? "validations"
    relations <- traverse A.decodeJson =<< obj .? "relations"
    inputType <- obj .? "inputType"
    pure $ InputConfig { inputType, relations, validations }

instance encodeJsonInputConfig
  :: (A.EncodeJson v, A.EncodeJson i, A.EncodeJson r) => A.EncodeJson (InputConfig v i r) where
  encodeJson (InputConfig { inputType, validations, relations }) = do
    "inputType" := inputType
      ~> "validations" := (A.encodeJson <$> validations)
      ~> "relations" := (A.encodeJson <$> relations)
      ~> A.jsonEmptyObject

-- Insert a new input into the form. Returns the created ref.
input :: ∀ v i r. i -> FormM v i r InputRef
input inputType = do
  (FormConfig f) <- get
  let ref = InputRef f.supply
  modify_ \(FormConfig form) -> wrap $ form
    { supply = form.supply + 1
    , inputs = Map.insert ref (wrap { inputType, relations: [], validations: [] }) form.inputs }
  pure ref

-- Augment an input with new validation. Returns the original ref.
validate :: ∀ v i r. v -> InputRef -> FormM v i r InputRef
validate validation ref = do
  let f = \(InputConfig v) -> pure $ wrap $ v { validations = validation : v.validations }
  modify_ \(FormConfig form) -> wrap $ form { inputs = Map.update f ref form.inputs }
  pure ref

-- Augment an input with a new relationship. Returns the original ref.
relate :: ∀ v i r. r -> InputRef -> FormM v i r InputRef
relate relation ref = do
  let f = \(InputConfig v) -> pure $ wrap $ v { relations = relation : v.relations }
  modify_ \(FormConfig form) -> wrap $ form { inputs = Map.update f ref form.inputs }
  pure ref
